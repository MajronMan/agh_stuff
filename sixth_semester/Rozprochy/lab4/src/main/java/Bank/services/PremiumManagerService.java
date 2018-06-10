package Bank.services;

import Bank.BankDatabase;
import Bank.BankLogic;
import Bank.CurrencyAdapter;
import org.apache.thrift.TException;
import sr.grpc.CurrencyNameG;
import sr.thrift.*;

import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

public class PremiumManagerService implements PremiumManager.Iface {

    private final BankDatabase database;
    private final Set<CurrencyNameT> currencies = new HashSet<>();

    public PremiumManagerService(BankDatabase database, Set<CurrencyNameG> currencies) {
        this.database = database;
        this.currencies.addAll(currencies.stream().map(CurrencyAdapter::adapt).collect(Collectors.toSet()));
    }

    @Override
    public LoanResponse askForLoan(LoanRequest loanRequest) throws InvalidGuid {
        if(!currencies.contains(loanRequest.currency)){
            return new LoanResponse(false);
        }
        return BankLogic.respond(loanRequest, database);
    }

    @Override
    public Money getBalance(GUID guid) throws InvalidGuid, TException {
        BankClient result = database.getBankClient(guid);

        if(result.type != AccountType.PREMIUM){
            InvalidGuid e = new InvalidGuid(guid, "Not a premium client!");
            e.printStackTrace();
            throw e;
        }else {
            return result.balance;
        }
    }
}
