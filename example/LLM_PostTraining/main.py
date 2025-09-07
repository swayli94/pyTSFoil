from collections.abc import Callable
import json
from pathlib import Path
import random
import re
from typing import Any, Iterator, Optional
import wandb
import torch
import torch.optim as optim
import torch.nn.functional as F
from torch.nn.utils import clip_grad_norm_
from torch.utils.data import DataLoader
from transformers import (
    PreTrainedTokenizer,
    GenerationConfig,
)
from modelscope import AutoModelForCausalLM, AutoTokenizer
from loss import approx_kl_divergence, GRPOLoss
from replay_buffer import ReplayBuffer, Experience, join_experience_batch
from env import Environment as FoilEnvironment
import argparse
import logging
from datetime import datetime



import os


def load_model(
    model_name_or_path: str,
    torch_dtype="auto",
    device_map="auto",
) -> tuple[AutoModelForCausalLM, PreTrainedTokenizer]:
    tokenizer = AutoTokenizer.from_pretrained(model_name_or_path)
    model = AutoModelForCausalLM.from_pretrained(
        model_name_or_path,
        torch_dtype=torch_dtype,
        device_map=device_map,
    )
    return model, tokenizer


def get_model_device(model):
    if hasattr(model, 'module'):
        return next(model.module.parameters()).device
    else:
        return next(model.parameters()).device


@torch.no_grad()
def rollout_foil(
    model: AutoModelForCausalLM,
    tokenizer: PreTrainedTokenizer,
    foil_env: FoilEnvironment,
    task_data: dict,
    num_rollouts: int,
    max_length: int = 1024,
    max_new_tokens: int = 4096,
    temperature: float = 0.8,
    top_p: float = 1.0,
) -> tuple[torch.Tensor, torch.Tensor, list[str]]:
    """
    使用FoilEnvironment环境进行rollout，实时评估奖励
    """
    model.eval()

    # 获取模型所在的设备
    model_device = get_model_device(model)

    prompt, initial_airfoil, history_actions = task_data['prompt'], task_data['initial_airfoil'], task_data['history_actions']

    # prompt, initial_airfoil, history_actions = foil_env.generate_prompt()
    
    messages = [
    {"role": "system", "content": "You are a foil expert, you can give the optimal action to improve the airfoil."},
    {"role": "user", "content": prompt}
    ]
    text = tokenizer.apply_chat_template(
        messages,
        tokenize=False,
        add_generation_prompt=True
    )

    model_inputs = tokenizer(
        [text],
        return_tensors="pt",
        padding=True,
        return_attention_mask=True,
        truncation=True,
        max_length=max_length - max_new_tokens,
    ).to(model_device)

    # duplicate prompt num_rollouts times
    model_inputs["attention_mask"] = model_inputs["attention_mask"].repeat(
        num_rollouts, 1
    )

    input_ids = model_inputs["input_ids"].repeat(num_rollouts, 1)
    model_inputs["input_ids"] = input_ids

    pad_token_id = tokenizer.eos_token_id
    
    generation_config = GenerationConfig(
        max_length=max_length,
        do_sample=True,
        max_new_tokens=max_new_tokens,
        top_p=top_p,
        temperature=temperature,
        eos_token_id=tokenizer.eos_token_id,  # 明确设置eos_token_id
    )
    sequence_ids = model.generate(**model_inputs, generation_config=generation_config)
    completions = tokenizer.batch_decode(
        sequence_ids[:, input_ids.shape[1] :], skip_special_tokens=True
    )

    action_mask = torch.zeros_like(sequence_ids, dtype=torch.bool)
    action_mask[:, input_ids.shape[1] :] = True
    action_mask[sequence_ids == pad_token_id] = False
    action_mask = action_mask[:, 1:]

    returns = torch.zeros(num_rollouts, 1, dtype=torch.float)


    
    for i, completion in enumerate(completions):
        # 使用foil环境评估输出质量
        # try:
        score = foil_env.verify_score(completion, initial_airfoil, history_actions)
        returns[i] = score
        # except Exception as e:
        #     print(f"Error evaluating completion {i}: {e}")
        #     returns[i] = 0.0

    return sequence_ids, returns.to(sequence_ids.device), action_mask, completions


def init_rng(seed: int) -> torch.Generator:
    random.seed(seed)
    return torch.manual_seed(seed)


def group_advantages(returns: torch.Tensor, eps: float = 1e-8) -> torch.Tensor:
    return (returns - returns.mean()) / (returns.std() + eps)


def sequence_log_probs_from_logits(
    logits: torch.tensor, output_ids: torch.tensor
) -> torch.Tensor:
    log_prob = F.log_softmax(logits, dim=-1)
    return log_prob.gather(dim=-1, index=output_ids.unsqueeze(-1)).squeeze(-1)


def sequences_log_probs(
    model: AutoModelForCausalLM,
    sequence_ids: torch.Tensor,
    attention_mask: torch.Tensor,
) -> torch.Tensor:
    position_ids = attention_mask.long().cumsum(dim=-1) - 1
    position_ids.masked_fill_(mask=(attention_mask == 0), value=1)
    output = model.forward(
        input_ids=sequence_ids,
        attention_mask=attention_mask,
        position_ids=position_ids,
        use_cache=False,
    )
    logits = output["logits"]
    log_probs = sequence_log_probs_from_logits(
        logits=logits[:, :-1].to(torch.float32),
        output_ids=sequence_ids[:, 1:],
    )
    return log_probs


def read_jsonl(file_name: str | Path) -> Iterator:
    file_path = Path(file_name)
    with file_path.open(mode="r", encoding="utf-8") as f:
        for line in f:
            yield json.loads(line)

def gen_foil_prompts(
    env: FoilEnvironment,
    max_rows: Optional[int] = None,
) -> list:
    """
    生成foil数据集
    """
    prompts = []
    for i in range(max_rows):
        prompt, initial_airfoil, history_actions = env.generate_prompt()
        prompts.append({"prompt": prompt, "initial_airfoil": initial_airfoil, "history_actions": history_actions})
    return prompts


def setup_logging(log_level: str = "INFO", log_file: Optional[str] = None):
    """设置日志配置"""
    level = getattr(logging, log_level.upper())
    
    # 创建formatter
    formatter = logging.Formatter(
        '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )
    
    # 配置root logger
    logger = logging.getLogger()
    logger.setLevel(level)
    
    # 清除现有的handlers
    logger.handlers.clear()
    
    # 控制台handler
    console_handler = logging.StreamHandler()
    console_handler.setLevel(level)
    console_handler.setFormatter(formatter)
    logger.addHandler(console_handler)
    
    # 文件handler（如果指定了log_file）
    if log_file:
        file_handler = logging.FileHandler(log_file)
        file_handler.setLevel(level)
        file_handler.setFormatter(formatter)
        logger.addHandler(file_handler)
    
    return logger

def parse_args():
    """解析命令行参数"""
    parser = argparse.ArgumentParser(description="Foil GRPO Training Script")
    
    # 模型参数
    parser.add_argument("--model_name", type=str, default="Qwen/Qwen2.5-7B-Instruct",
                        help="Model name or path")
    parser.add_argument("--checkpoint_path", type=str, default="./output_foil",
                        help="Path to save checkpoints")
    
    # 训练参数
    parser.add_argument("--seed", type=int, default=42, help="Random seed")
    parser.add_argument("--lr", type=float, default=3e-6, help="Learning rate")
    parser.add_argument("--kl_weight", type=float, default=0.01, help="KL weight")
    parser.add_argument("--clip_eps", type=float, default=0.2, help="Clipping epsilon")
    parser.add_argument("--max_norm", type=float, default=1.0, help="Max gradient norm")
    
    # 数据参数
    parser.add_argument("--max_rows", type=int, default=32*1024,
                        help="Maximum number of training samples")
    parser.add_argument("--rollouts_per_step", type=int, default=16,
                        help="Number of rollouts per training step")
    parser.add_argument("--group_size", type=int, default=8,
                        help="Group size for GRPO")
    parser.add_argument("--train_batch_size", type=int, default=4,
                        help="Training batch size")
    parser.add_argument("--epochs_per_step", type=int, default=1,
                        help="Epochs per training step")
    
    # 生成参数
    parser.add_argument("--max_length", type=int, default=20000,
                        help="Maximum sequence length")
    parser.add_argument("--max_new_tokens", type=int, default=4096,
                        help="Maximum new tokens")
    parser.add_argument("--temperature", type=float, default=0.8,
                        help="Generation temperature")
    parser.add_argument("--top_p", type=float, default=0.9,
                        help="Top-p sampling parameter")
    
    # 其他参数
    parser.add_argument("--checkpoint_interval", type=int, default=20,
                        help="Checkpoint save interval")
    parser.add_argument("--wandb_project", type=str, default=None,
                        help="Weights & Biases project name")
    parser.add_argument("--log_level", type=str, default="INFO",
                        choices=["DEBUG", "INFO", "WARNING", "ERROR"],
                        help="Logging level")
    parser.add_argument("--log_file", type=str, default=None,
                        help="Log file path")
    parser.add_argument("--test_only", action="store_true",
                        help="Run test only without training")
    
    return parser.parse_args()


def main():
    args = parse_args()
    
    # 设置日志
    log_file = args.log_file or f"train_foil_{datetime.now().strftime('%Y%m%d_%H%M%S')}.log"
    logger = setup_logging(args.log_level, log_file)
    logger.info("Starting foil GRPO Training")
    logger.info(f"Arguments: {args}")
    
    # 使用args中的参数
    seed = args.seed
    wandb_project = args.wandb_project
    model_name = args.model_name
    checkpoint_path = Path(args.checkpoint_path)
    checkpoint_interval = args.checkpoint_interval
    
    num_gpus = torch.cuda.device_count()
    train_batch_size = args.train_batch_size * max(1, num_gpus // 2)
    lr = args.lr
    kl_weight = args.kl_weight
    clip_eps = args.clip_eps
    
    group_size = args.group_size * max(1, num_gpus // 4)
    rollouts_per_step = args.rollouts_per_step
    epochs_per_step = args.epochs_per_step
    max_norm = args.max_norm
    
    max_length = args.max_length
    max_new_tokens = args.max_new_tokens
    top_p = args.top_p
    temperature = args.temperature
    
    logger.info(f"Available GPUs: {num_gpus}")
    logger.info(f"Adjusted train_batch_size: {train_batch_size}")
    logger.info(f"Adjusted group_size: {group_size}")
    
    init_rng(seed)

    # 初始化foil环境
    logger.info("Initializing foil environment...")
    env = FoilEnvironment(data_file_path="example/env_FigState_MultiBump/data")

    # 加载模型
    logger.info("Loading models...")
    reference_model, _ = load_model(model_name, device_map="auto")
    model, tokenizer = load_model(model_name, device_map="auto")
    
    # 多GPU设置
    if num_gpus > 1:
        if not hasattr(model, 'hf_device_map') or len(model.hf_device_map) == 1:
            logger.info(f"Using DataParallel with {num_gpus} GPUs")
            model = torch.nn.DataParallel(model)
            reference_model = torch.nn.DataParallel(reference_model)
        else:
            logger.info(f"Model automatically distributed across GPUs: {model.hf_device_map}")
    cpu_device = torch.device("cpu")

    optimizer = optim.Adam(model.parameters(), lr=lr)
    logger.info("Models loaded successfully")

    reference_model.eval()
    model.gradient_checkpointing_enable(
        gradient_checkpointing_kwargs={"use_reentrant": False}
    )

    pad_token_id = tokenizer.eos_token_id

    prompts = gen_foil_prompts(env, args.max_rows)
    logger.info(f"Generated {len(prompts)} foil prompts")
    
    if len(prompts) == 0:
        logger.error("No prompts found! Please check traj data")
        return
    
    def custom_collate_fn(batch):
        """Custom collate function that returns batch as-is without stacking"""
        return batch
    
    prompt_loader = DataLoader(
        prompts,
        batch_size=rollouts_per_step,
        shuffle=True,
        drop_last=True,
        pin_memory=False,
        collate_fn=custom_collate_fn,
    )

    replay_buffer = ReplayBuffer()
    objective = GRPOLoss(clip_eps=clip_eps, kl_weight=kl_weight)

    if wandb_project is None:
        wandb.init(mode="disabled")
        logger.info("Weights & Biases disabled")
    else:
        wandb.init(project=wandb_project)
        logger.info(f"Weights & Biases initialized with project: {wandb_project}")

    logger.info("Starting training loop...")
    for k, prompt_batch in enumerate(prompt_loader):
        logger.info(f"Starting step {k}")
        rollout_returns = []
        replay_buffer.clear()

        with torch.no_grad():
            for task_idx, task_data in enumerate(prompt_batch):
                logger.debug(f"Processing task {task_idx + 1}/{len(prompt_batch)}")
                
                sequence_ids, returns, action_mask, completions = rollout_foil(
                    model,
                    tokenizer,
                    env,
                    task_data,
                    num_rollouts=group_size,
                    max_length=max_length,
                    max_new_tokens=max_new_tokens,
                    temperature=temperature,
                    top_p=top_p,
                )

                logger.info(
                    f"Rollout completed - returns_sum={returns.sum().item():.2f}, "
                    f"replay_buffer_size={len(replay_buffer)}, "
                    f"sequence_shape={sequence_ids.shape}, "
                    # f"moves={len(task_data['moves'])}"
                )
                rollout_returns.append(returns.cpu())

                # 检查是否有有效的返回值
                if returns.sum().item() == 0:
                    logger.warning("All returns are 0, skipping this batch")
                    continue

                advantages = group_advantages(returns)
                attention_mask = sequence_ids != pad_token_id

                log_probs = sequences_log_probs(
                    model=model,
                    sequence_ids=sequence_ids,
                    attention_mask=attention_mask,
                )
                log_probs_ref = sequences_log_probs(
                    model=reference_model,
                    sequence_ids=sequence_ids,
                    attention_mask=attention_mask,
                )
                kl = approx_kl_divergence(
                    log_probs=log_probs,
                    log_probs_ref=log_probs_ref,
                    action_mask=action_mask,
                )

                experience = Experience(
                    sequences=sequence_ids,
                    action_log_probs=log_probs,
                    log_probs_ref=log_probs_ref,
                    returns=returns,
                    advantages=advantages,
                    attention_mask=attention_mask,
                    action_mask=action_mask,
                    kl=kl,
                )
                replay_buffer.append(experience.to(cpu_device))

        torch.cuda.empty_cache()
        
        if len(rollout_returns) == 0:
            logger.warning(f"No valid rollouts for step {k}, skipping training")
            continue
            
        episode_return_sum = torch.stack(rollout_returns).sum()
        logger.info(f"Step {k} completed - total_returns={episode_return_sum:.4f}")
        wandb.log({"returns": episode_return_sum})

        if len(replay_buffer) == 0:
            logger.warning(f"Empty replay buffer for step {k}, skipping training")
            continue

        experience_sampler = DataLoader(
            replay_buffer,
            batch_size=train_batch_size,
            shuffle=True,
            drop_last=True,
            collate_fn=join_experience_batch,
        )

        logger.info(f"Starting training for step {k}")
        for step_epoch in range(epochs_per_step):
            model.train()
            
            # 获取模型所在的设备
            model_device = get_model_device(model)

            batch_count = 0
            for exp in experience_sampler:
                exp: Experience
                exp = exp.to(model_device)
                batch_count += 1

                optimizer.zero_grad()

                log_probs = sequences_log_probs(
                    model, sequence_ids=exp.sequences, attention_mask=exp.attention_mask
                )

                loss, kl = objective(log_probs=log_probs, experience=exp)

                if not loss.isfinite():
                    logger.warning(f"Loss not finite, skipping backward, loss={loss}")
                    logger.debug(f"experience.advantages={exp.advantages}")
                    continue

                loss.backward()
                grad_norm = clip_grad_norm_(model.parameters(), max_norm=max_norm)
                logger.info(
                    f"Epoch {step_epoch}, Batch {batch_count}: "
                    f"kl={kl:.4f}, grad_norm={grad_norm:.4f}, loss={loss:.4f}"
                )
                wandb.log({"kl": kl, "grad_norm": grad_norm, "loss": loss})

                optimizer.step()

        logger.info(f"Step {k} training completed")

        if (
            checkpoint_path is not None
            and checkpoint_interval is not None
            and (k + 1) % checkpoint_interval == 0
        ):
            checkpoint_dir = checkpoint_path / f"step_{k}"
            model.save_pretrained(checkpoint_dir)
            logger.info(f"Checkpoint saved to {checkpoint_dir}")

    if checkpoint_path is not None:
        final_checkpoint = checkpoint_path / f"step_{k}"
        model.save_pretrained(final_checkpoint)
        logger.info(f"Final checkpoint saved to {final_checkpoint}")

    logger.info("Training completed successfully")


def test_rollout_foil():
    """
    测试函数：读取一条foil数据并调用rollout_foil
    """
    print("Starting test_rollout_foil...")
    args = parse_args()
    
    # 基本参数设置
    seed = 42
    # 使用多GPU配置而不是单GPU
    # device_index = 0  # 注释掉单GPU设置
    model_name = args.model_name
    max_length = args.max_length
    top_p = 0.1
    temperature = 0.2
    num_rollouts = 4  # 测试用较小的rollouts数量
    max_new_tokens = args.max_new_tokens
    
    # 初始化
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    num_gpus = torch.cuda.device_count()
    print(f"Available GPUs: {num_gpus}")
    
    init_rng(seed)
    
    print("Loading model and tokenizer...")
    # 加载模型和tokenizer - 使用auto device_map支持多GPU
    model, tokenizer = load_model(model_name, device_map="auto")
    
    # 如果有多个GPU但模型没有自动分布，使用DataParallel
    if num_gpus > 1:
        if not hasattr(model, 'hf_device_map') or len(model.hf_device_map) == 1:
            print(f"Using DataParallel with {num_gpus} GPUs")
            model = torch.nn.DataParallel(model)
        else:
            print(f"Model automatically distributed across GPUs: {model.hf_device_map}")
    
    print("Initializing foil environment...")
    # 初始化foil环境
    env = FoilEnvironment(data_file_path="example/env_FigState_MultiBump/data")
    
    print("Generating foil prompt...")
    # 读取一条foil数据
    prompts, initial_airfoil, history_actions = env.generate_prompt()
    
    if len(prompts) == 0:
        print("No foil prompts found!")
        return
    print(f"Test data: prompt={prompts}, initial_airfoil={initial_airfoil}, history_actions={history_actions}")
        
    
    print("Running rollout_foil...")
    # 调用rollout_foil
    try:
        with torch.no_grad():
            sequence_ids, returns, action_mask, completions = rollout_foil(
                model=model,
                tokenizer=tokenizer,
                foil_env=env,
                num_rollouts=num_rollouts,
                max_length=max_length,
                max_new_tokens=max_new_tokens,
                temperature=temperature,
                top_p=top_p,
            )
        
        print(f"Rollout completed successfully!")
        print(f"Generated {len(completions)} completions")
        print(f"Returns: {returns.flatten().tolist()}")
        print(f"Total return: {returns.sum().item():.4f}")
        print(f"Average return: {returns.mean().item():.4f}")
        
        # 显示生成的完成示例
        for i, (completion, ret) in enumerate(zip(completions, returns.flatten())):
            print(f"\nCompletion {i+1} (score: {ret.item():.4f}):")
            print(f"'{completion[:200]}{'...' if len(completion) > 200 else ''}'")
            
    except Exception as e:
        print(f"Error during rollout: {e}")
        import traceback
        traceback.print_exc()
    
    print("Test completed!")


if __name__ == "__main__":
    args = parse_args()
    # setting visible devices
    import os
    os.environ["CUDA_VISIBLE_DEVICES"] = "1,2,3,4,5,6"
    
    if args.test_only:
        # 运行测试
        test_rollout_foil()
    else:
        # 运行完整训练
        main()