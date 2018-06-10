package Bank.services;

import Bank.BankDatabase;
import Bank.BankLogic;
import org.apache.thrift.TException;
import sr.thrift.*;

import java.util.UUID;

public class AccountCreatorService implements sr.thrift.AccountCreator.Iface {

    private final BankDatabase database;

    public AccountCreatorService(BankDatabase database){
        this.database = database;
    }

    @Override
    public BankClient registerClient(PersonalData data, Money initialBalance) throws TException {
        System.out.println("Trying to register " + data.name + " " + data.surname);
        AccountType type = BankLogic.isPremium(data.salary)? AccountType.PREMIUM : AccountType.STANDARD;

        UUID uuid = UUID.randomUUID();
        BankClient newClient = new BankClient(data, type, new GUID(uuid.toString()), initialBalance);
        database.register(newClient);
        return newClient;
    }
}
