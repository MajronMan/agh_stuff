package messages;

import messages.MessageType;

import java.io.Serializable;

public class Request implements Serializable {
    public final MessageType type;
    public final byte[] payload;

    public Request(MessageType type, byte[] payload) {
        this.type = type;
        this.payload = payload;
    }
}
