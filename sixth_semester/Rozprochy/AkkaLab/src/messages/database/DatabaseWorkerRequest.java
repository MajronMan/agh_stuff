package messages.database;

import java.io.Serializable;

public class DatabaseWorkerRequest implements Serializable {
    public final String databaseName;
    public final String phrase;

    public DatabaseWorkerRequest(String databaseName, String phrase) {
        this.databaseName = databaseName;
        this.phrase = phrase;
    }
}
