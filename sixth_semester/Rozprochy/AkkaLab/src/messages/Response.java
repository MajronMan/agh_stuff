package messages;

import java.io.Serializable;

public class Response implements Serializable {
    public final boolean success;
    public final byte[] payload;

    public Response(boolean success, byte[] payload) {
        this.success = success;
        this.payload = payload;
    }
}
