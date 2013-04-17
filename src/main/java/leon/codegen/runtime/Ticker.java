package leon.codegen.runtime;

public class Ticker {
    private int ticksLeft;

    public Ticker(int maxTicks) {
        this.ticksLeft = maxTicks;
    }

    public void tick() throws LeonCodeGenEvaluationException {
        if(ticksLeft <= 0) {
            throw new LeonCodeGenEvaluationException("Maximum number of evaluation steps reached.");
        }
        ticksLeft--;
    }
}
