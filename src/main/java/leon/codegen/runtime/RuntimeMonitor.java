package leon.codegen.runtime;

public class RuntimeMonitor {
    private final boolean evaluateContracts;
    private int ticksLeft;

    public RuntimeMonitor(boolean evaluateContracts, int maxTicks) {
        this.evaluateContracts = evaluateContracts;
        this.ticksLeft = maxTicks;
    }

    public void tick() throws LeonCodeGenEvaluationException {
        if(ticksLeft < 0) {
            return;
        } else if(ticksLeft == 0) {
            throw new LeonCodeGenEvaluationException("Maximum number of evaluation steps reached.");
        } else {
          ticksLeft--;
        }
    }

    public boolean shouldEvaluateContracts() {
        return evaluateContracts;
    }
}
