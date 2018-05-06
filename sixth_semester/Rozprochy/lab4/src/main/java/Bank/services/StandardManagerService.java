package Bank.services;

import Bank.BankDatabase;
import org.apache.thrift.TException;
import sr.thrift.*;

public class StandardManagerService implements StandardManager.Iface {

    private final BankDatabase database;

    public StandardManagerService(BankDatabase database) {
        this.database = database;
    }

    @Override
    public Money getBalance(GUID guid) throws InvalidGuid, TException {
        BankClient result = database.getBankClient(guid);
        if(result.type != AccountType.STANDARD){
            InvalidGuid e = new InvalidGuid(guid, "Not a standard client!");
            e.printStackTrace();
            throw e;
        } else {
            return result.balance;
        }
    }
}
