package comm.message.response;

import comm.message.Message;
import comm.message.MessageType;

import java.util.Arrays;

/**
 * Created by jimmywest on 2014-12-05.
 */
public class Error extends Message {

    private String error;
    public Error(byte id, byte[] data) {
        super(id, MessageType.ERROR, data);
        error = Arrays.toString(data);
    }

    public String getError() {
        return error;
    }
}
