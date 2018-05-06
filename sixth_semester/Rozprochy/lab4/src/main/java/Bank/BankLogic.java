package Bank;

import sr.thrift.*;

public class BankLogic {
    private static final Money premiumThreshold = new Money(5000 * 100);
    private static final double loanInterest = 0.15;

    public static boolean isPremium(Money salary){
        return salary.cents >= premiumThreshold.cents;
    }

    public static LoanResponse respond(LoanRequest request, BankDatabase database) throws InvalidGuid {
        BankClient client = database.getBankClient(request.guid);
        if(isPremium(client.data.salary)){
            LoanResponse response = new LoanResponse(true);

            double withInterest = request.amount.cents + loanInterest * request.amount.cents;
            double targetWithInterest = withInterest * database.getRate(request.currency);

            response.baseCost = new Money(Math.round(withInterest));
            response.targetCost = new Money(Math.round(targetWithInterest));

            return response;
        } else {
            return new LoanResponse(false);
        }
    }
}
