# Training process

## Airfoil database

The 25 selected airfoils are used as the initial airfoils.
Each airfoil is assigned to eight environments, resulting in 200 environments.

## Phase 1

```python

n_envs = 100

ppo_agent = PPO_Custom_MultiEnv(
    env_fns=env_fns,
    env_eval=eval_env,
    lr=1e-5,
    gamma=0.99,
    gae_lambda=0.98,
    clip_epsilon=0.8,
    value_loss_coef=0.5,
    entropy_coef=0.1,
    max_grad_norm=0.5,
    n_epochs=10,
    batch_size=2000,
    n_steps=10,
    dim_latent=128,
    dim_hidden=1024,
    n_interp_points=101,
    initial_action_std=0.5,
    device=device,
    max_processes=50
)

```

## Phase 2



