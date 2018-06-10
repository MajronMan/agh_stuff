package messages;

import java.io.Serializable;

public class WorkRequest implements Serializable {
    public final byte[] payload;

    public WorkRequest(byte[] payload) {
        this.payload = payload;
    }
}
