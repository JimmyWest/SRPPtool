package comm.message.response;

import comm.message.Message;
import comm.message.MessageType;

import java.util.Arrays;

/**
 * Created by jimmywest on 2014-12-05.
 */
public class ErrorNOID extends Message {

    private String error;
    public ErrorNOID(byte[] data) {
        super(MessageType.ERROR, data);
        error = readString();
    }

    public String getError() {
        return error;
    }
}
