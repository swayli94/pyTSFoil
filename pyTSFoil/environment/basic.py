'''
Environment, state, action, and trajectory classes for reinforcement learning and LLM
'''
import copy
import json
import numpy as np
from typing import List, Tuple, Any

import concurrent
import concurrent.futures
from concurrent.futures import as_completed


class NumpyArrayEncoder(json.JSONEncoder):
    '''
    Convert ndarray to list for json dump
    '''
    def default(self, o: Any) -> Any:
        if isinstance(o, np.ndarray):
            return o.tolist()
        return super().default(o)


class Action():
    '''
    Action of airfoil geometric modifications
    
    Parameters
    ------------------
    dim_action: int
        dimension of action
        
    action_upper_bound, action_lower_bound: ndarray [dim_action], or None
        upper and lower bounds of the action vector
        
    EPSILON: float
        small value to avoid zero in the denominator
    
    Attributes
    ------------------
    action_name: list
        name of the action components
        
    action_upper_bound, action_lower_bound: ndarray [dim_action]
        upper and lower bounds of the action vector
    
    '''
    def __init__(self, dim_action: int, action_upper_bound=None, action_lower_bound=None,
                    action_name: List[str]=None, EPSILON=1E-20) -> None:
        
        self.dim_action = dim_action
        
        if action_name is None:
            self.action_name : List[str] = [None for _ in range(dim_action)]
        else:  
            self.action_name = action_name
        
        if action_upper_bound is None:
            self.action_upper_bound = np.zeros(dim_action)
        else:
            self.action_upper_bound = action_upper_bound
            
        if action_lower_bound is None:
            self.action_lower_bound = np.ones(dim_action)
        else:
            self.action_lower_bound = action_lower_bound
            
        for i in range(self.dim_action):
            self.action_upper_bound[i] = max(self.action_upper_bound[i], self.action_lower_bound[i]+EPSILON)

    def apply_bounds(self, action_array: np.ndarray) -> bool:
        '''
        Apply bounds to the action

        Parameters
        --------------
        action_array: ndarray
            unscaled action vector

        Example
        --------------
        >>> within_bounds = apply_bounds(action_array)
        '''
        within_bounds = True

        for i in range(self.dim_action):

            if action_array[i] > self.action_upper_bound[i]:
                action_array[i] = self.action_upper_bound[i]
                within_bounds = False

            elif action_array[i] < self.action_lower_bound[i]:
                action_array[i] = self.action_lower_bound[i]
                within_bounds = False

        return within_bounds

    def scale_action(self, action_array: np.ndarray) -> np.ndarray:
        '''
        Scale action to [0,1]
        '''
        return (action_array - self.action_lower_bound)/(self.action_upper_bound-self.action_lower_bound)

    def recover_action(self, scaled_action_array: np.ndarray) -> np.ndarray:
        '''
        Recover action from [0,1]
        '''
        return scaled_action_array*(self.action_upper_bound-self.action_lower_bound) + self.action_lower_bound

    def random_action(self) -> np.ndarray:
        '''
        Take a random action
        
        Returns
        ---------------
        action_array: ndarray
            an unscaled action vector
        '''
        action_array = np.random.rand(self.dim_action)
        
        action_array = self.recover_action(action_array)

        return action_array

    def apply_action(self, scaled_action_array: np.ndarray, *args):
        '''
        Apply action
        
        Parameters
        ---------------
        scaled_action_array: ndarray
            scaled action vector
        '''
        raise NotImplementedError

    @staticmethod
    def within_0_and_1(value: float) -> bool:
        return value >= 0.0 and value <= 1.0


class State():
    '''
    State variables
    '''
    def __init__(self) -> None:
        
        self.state_name : List[str] = []
        
        self.state_array : np.ndarray = None
        
    @property
    def dim_state(self) -> int:
        '''
        Dimension of the state vector.
        '''
        return len(self.state_name)

    def __call__(self, key=None) -> float:
        '''
        Fetch the state variable (vector).
        
        Parameters
        ---------------
        key: str
            key string for the state variable
        '''
        return self.fetch(key)
        
    def fetch(self, key=None) -> float:
        '''
        Fetch the state variable (vector).
        
        Parameters
        ---------------
        key: str
            key string for the state variable
        '''
        if key is None:
            return self.state_array
        
        elif isinstance(key, str):
            
            ii = self.state_name.index(key)
            return self.state_array[ii]
        
        else:
            print('Error [State.fetch()]: ')
            print('    the input [key] must be None or a key string in [state_name]', key)
            print()
            raise Exception
        
    def update(self):
        raise NotImplementedError


class Trajectory():
    '''
    In each step, the environment takes an action and returns a reward.
    The data of environment is stored in the trajectory.
    
    The data flow is as follows:
    
        state (reference step) > action (current step) > reward & state (current step) > ...
    
    
    Parameters
    ----------------
    action_name: List[str]
        A list of action names
        
    state_name: List[str]
        A list of state names
        
    aux_data_name: List[str]
        A list of auxiliary data names to be written in the summary file

   
    Attributes
    ----------------
    key_list: List[str]
    
        A list of keys for the data stored in each step.
        It contains:
        
        - 'step' (the current step of trajectory)
        - 'valid' (whether the step is valid or not)
        - 'reference-step' (the reference step, which is usually the previous step)
        - 'action' (the action in the current step, which is applied to the state of reference step)
        - 'state' (the current state, resulting from the action of the current step)
        - 'reward' (the current reward, resulting from the action of the current step)
        - 'total-reward' (the accumulative reward up to the current step)
    
    data: List[dict]
        A list of dictionaries that store data in each step
    
    '''
    def __init__(self, action_name: List[str], state_name: List[str], aux_data_name: List[str] = []) -> None:
        
        #* Parameters
        self.action_name = action_name
        self.state_name = state_name
        self.aux_data_name = aux_data_name
        
        #* Attributes
        self.data : List[dict] = []
        
        self.key_list: List[str] = ['step', 'valid', 'reference-step', 'action', 'state', 'reward', 'total-reward']

    @property
    def num_steps(self) -> int:
        '''
        Number of steps, i.e., the length of `data`.
        '''
        return len(self.data)
    
    def check_valid_key(self, key: str) -> bool:
        '''
        Check whether the key is in the user-defined key list.
        
        Parameters
        ---------------
        key: str
            key string for the data
        '''
        return key in self.key_list

    def __call__(self, step: int, key=None) -> Any:
        '''
        Fetch the data in a given step.
        
        Parameters
        ---------------
        step: int
            the step of the data
            
        key: str
            key string for the data
        '''
        return self.fetch(step, key)

    def save(self, step: int, key: str, value: Any) -> None:
        '''
        Save data of a certain step.
        
        Parameters
        ---------------
        step: int
            the step of this data
            
        key: str
            key string for the data
            
        value: Any
            data value
        '''
        if step<0 or step>=self.num_steps+1:
            print('Error [Trajectory.save()]: ')
            print('    the input [step] is negative or larger than the current step plus one', step, self.num_steps+1)
            print()
            raise Exception
        
        elif step==self.num_steps:
            
            if self.check_valid_key(key):
                self.data.append({'step': step})
                self.data[step][key] = copy.deepcopy(value)
                
            else:
                print('Error [Trajectory.save()]: ')
                print('    the input [key] is not valid or not in the user-defined key list', key)
                print()
                raise Exception

        else:
            
            self.data[step][key] = copy.deepcopy(value)

    def fetch(self, step=None, key=None) -> Any:
        '''
        Fetch the data in a given step.
        
        Parameters
        ---------------
        step: int, or None
            the step of the data.
            If None, then use the last step.
            
        key: str, or None
            key string for the data.
            If None, then return the whole dictionary of the step.
        '''
        if step is None:
            step = self.num_steps - 1
        
        if step<0 or step>=self.num_steps:
            print('Error [Trajectory.fetch()]: ')
            print('    the input [step] is negative or larger than the current step', step, self.num_steps)
            print()
            raise Exception
        
        if key is None:
            return self.data[step]
        
        elif isinstance(key, str):
            return self.data[step][key]
        
        else:
            print('Error [Trajectory.fetch()]: ')
            print('    the input [key] must be None or a key string of the dictionary [data]', key)
            print()
            raise Exception

    def save2json(self, fname_json: str) -> None:
        '''
        Save the trajectory in a json format file.
        '''
        with open(fname_json, 'w') as f:
            json.dump(self.data, f, indent=4, cls=NumpyArrayEncoder)
            
    def load_json(self, fname_json: str) -> None:
        '''
        Load the trajectory from a json format file.
        '''
        with open(fname_json, 'r') as file:
            self.data = json.load(file)

    def save_summary(self, fname_summary: str) -> None:
        '''
        Save summary of the trajectory to file.
        '''
        with open(fname_summary, 'w') as f:
            
            f.write('Variables= step          valid reference-step         reward   total-reward')
            for name in self.action_name:
                f.write(' %14s'%(name))
            for name in self.state_name:
                f.write(' %14s'%(name))
            for name in self.aux_data_name:
                f.write(' %14s'%(name))
            f.write('\n')

            for i_step in range(self.num_steps):
                
                f.write(' %14d %14d'%(i_step, self.fetch(i_step, 'valid')))
                f.write(' %14d'%(self.fetch(i_step, 'reference-step')))
                
                #* Reward
                f.write(' %14.6E'%(self.fetch(i_step, 'reward')))
                f.write(' %14.6E'%(self.fetch(i_step, 'total-reward')))
                
                #* Action
                if i_step == 0:
                    action = np.zeros(len(self.action_name))
                else:
                    action = self.fetch(i_step, 'action')
                
                for i in range(action.shape[0]):
                    f.write(' %14.6E'%(action[i]))

                #* State
                state = self.fetch(i_step, 'state')

                for i in range(len(self.state_name)):
                    f.write(' %14.6E'%(state[i]))

                #* Auxiliary Data
                for name in self.aux_data_name:
                    
                    value = self.fetch(i_step, name)
                    if not isinstance(value, float):
                        value = 0.0
                        
                    f.write(' %14.6E'%(value))

                f.write('\n')


class Environment():
    '''
    Environment class
    
    In each step, the environment takes an action and returns a reward.
    The data of environment is stored in the trajectory.
    
    The data flow is as follows:
    
        state (reference step) > action (current step) > reward & state (current step) > ...
    
    '''
    def __init__(self, ID=0, name='AbstractEnv') -> None:
        
        #* Parameters
        self.name = name
        self.ID = ID
        
        #* Attributes
        self.action : Action = None
        self.state : State = None
        self.trajectory : Trajectory = None
        
        self.reward = 0.0
        self.total_reward = 0.0
        
        self._reference_step = 0
        
        #* Flags
        self.is_prepared = False
        self.is_stopped = False
        
        #* Optional attributes
        self.path = '.'
        self.penalty_reward = 0.0
    
    @property
    def num_steps(self) -> int:
        '''
        The number of steps in the trajectory, i.e., the number of steps taken so far.
        '''
        return self.trajectory.num_steps
    
    @property
    def current_step(self) -> int:
        '''
        The current step, i.e., the index of `step` data in `trajectory`.
        '''
        return self.num_steps - 1
    
    @property
    def reference_step(self) -> int:
        '''
        The reference step, i.e., the index of `step` data in `trajectory`.
        The action is taken on the state of the reference step.
        The action, reward, and the new state are stored in the current step.
                
        - The initial step is 0.
        - The reference step is usually the previous step, unless `undo` is called.
        '''
        return self._reference_step
        
    def prepare(self) -> None:
        '''
        Setup the environment for the first time
        
        Must implement this based on the problem.
        '''
        if self.is_prepared:
            return
        else:
            self.is_prepared = True
            
        self._reference_step = 0

        #* Define the action, trajectory

        #* Define the initial step

        raise NotImplementedError

    def reset(self) -> None:
        '''
        Reset to the initial state
        '''
        self.reward = 0.0
        self.total_reward = 0.0
        
        self._reference_step = 0
        
        self.is_stopped = False
        
        self.trajectory.data = self.trajectory.data[:1]

    def step(self) -> None:
        '''
        Take one step
        '''
        raise NotImplementedError

    def update_reference_step(self) -> None:
        '''
        Update the reference step to the current step.
        '''
        self._reference_step = self.current_step

    def undo_last_step(self) -> None:
        '''
        Undo the last step. Run before `step()` is called.
        
        Note: When the `step()` has not been called, the `current_step` is actually the last step.
        '''
        self.is_stopped = False
        self.trajectory.save(self.current_step, 'valid', False)
        self._reference_step = self.trajectory.fetch(self.current_step, 'reference-step')

    def undo_current_step(self) -> None:
        '''
        Undo current step. Run after `step()` is called.
        
        Note: After the `step()` is called, the `current_step` is the current step.
        '''
        self.is_stopped = False
        self.trajectory.save(self.current_step, 'valid', False)
        self._reference_step = self.trajectory.fetch(self.current_step, 'reference-step')

    def get_total_reward(self) -> float:
        '''
        Get the total reward
        '''
        total_reward = 0.0
        for i_step in range(self.num_steps):
            total_reward = max(total_reward, self.trajectory.fetch(i_step, 'total-reward'))
        
        return total_reward 
    
    def generate_episode(self, policy: callable, max_steps=100) -> None:
        '''
        Generate an episode (a trajectory) by taking steps until the episode is done.
        
        Must implement this based on the problem.
        
        >>> action = policy(state, **kwargs)
        '''
        raise NotImplementedError


class ParallelEnvironments():
    '''
    Multiple environments running in parallel using the multiprocessing module.
    '''
    def __init__(self, envs: List[Environment], n_process=None, timeout=None) -> None:
        
        self.envs = envs
        
        self.n_process = n_process
        self.timeout = timeout
    
    @property
    def n_env(self) -> int:
        '''
        The number of environments
        '''
        return len(self.envs)
    
    @property
    def average_total_reward(self) -> float:
        '''
        The average total reward of the environments
        '''
        return np.mean([env.total_reward for env in self.envs])
    
    @staticmethod
    def mp_function(i_env: int, env: Environment) -> Tuple[bool, int, Environment]:
        '''
        Multiprocessing function
        '''
        env.step()
        
        return True, i_env, env
    
    def reset(self) -> None:
        '''
        Reset to the initial state
        '''
        for env in self.envs:
            env.reset()
    
    def step(self) -> None:
        '''
        Evaluate the environments in parallel

        Notes
        -----------------
        Schedule the callable functions to be executed

        >>> future = executor.submit(fn, *args, **kwargs)

        returns a Future object representing the execution of the callable

        Yield futures as they complete (finished or cancelled)

        >>> for f in as_completed(futures, timeout=None):
        >>>     f.result()

        Any futures that completed before as_completed() is called will be yielded first. 
        The returned iterator raises a concurrent.futures.TimeoutError 
        if __next__() is called and the result isn't available after timeout seconds 
        from the original call to as_completed(). timeout can be an int or float. 
        If timeout is not specified or None, there is no limit to the wait time.
        #! This timeout will raise an Error
        '''
        succeeds = [True]*self.n_env
        
        #* Serial calculation
        if self.n_process==None:

            for i_env, env in enumerate(self.envs):
                env.step()

        #* Multiprocessing calculation
        else:

            with concurrent.futures.ProcessPoolExecutor(max_workers=self.n_process) as executor:

                futures = []
                
                for i_env, env in enumerate(self.envs):
                    
                    futures.append(executor.submit(self.mp_function, i_env, env))

                for f in as_completed(futures, timeout=self.timeout):

                    succeed, i_env = f.result()
                    
                    succeeds[i_env] = succeed
  
                self.executor = executor

    def finalize(self) -> None:
        '''
        Signal the executor that it should free any resources.

        Calls to Executor.submit() and Executor.map() made 
        after shutdown will raise RuntimeError.

        >>> executor.shutdown(wait=True)
        '''
        self.executor.shutdown(wait=True)

