
pub struct StateHandlerResult<State, Output> {
    next_state: State,
    output: Option<Output>,
}

pub trait StateHandler<State> {
    type Output;

    fn handle_state(&self, state: State) -> StateHandlerResult<State, Self::Output>;

    fn make_result(next_state: State) -> StateHandlerResult<State, Self::Output> {
        StateHandlerResult { next_state: next_state, output: None }
    }

    fn make_result_with_output(next_state: State, output: Self::Output)
            -> StateHandlerResult<State, Self::Output> {
        StateHandlerResult { next_state: next_state, output: Some(output) }
    }
}

pub struct StateMachine<'a, State, Handler: 'a> {
    state: State,
    state_handler: &'a Handler,
}

impl<'a, State: Eq + Copy, Handler: StateHandler<State> + 'a> StateMachine<'a, State, Handler> {
    pub fn new(initial_state: State, state_handler: &'a Handler) -> Self {
        StateMachine {
            state: initial_state,
            state_handler: state_handler,
        }
    }

    pub fn step(&mut self) -> Option<Handler::Output> {
        let result = self.state_handler.handle_state(self.state);
        self.state = result.next_state;
        result.output
    }

    pub fn run_until_output(&mut self) -> Handler::Output {
        loop {
            if let Some(output) = self.step() {
                return output;
            }
        }
    }

    pub fn run_until_state(&mut self, final_state: State) -> Vec<Handler::Output> {
        let mut outputs: Vec<Handler::Output> = vec![];
        while self.state != final_state {
            if let Some(output) = self.step() {
                outputs.push(output);
            }
        }
        outputs
    }
}
