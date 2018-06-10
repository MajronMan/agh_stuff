package messages.database;

import java.io.Serializable;

public class DatabaseSupervisorRequest implements Serializable {
    public final String phrase;

    public DatabaseSupervisorRequest(String phrase) {
        this.phrase = phrase;
    }
}
