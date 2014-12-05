package comm.message.response;

import comm.message.Message;
import comm.message.MessageType;

/**
 * Created by jimmywest on 2014-12-05.
 */
public class Reject extends Message {
    public Reject(byte id) {
        super(id, MessageType.REJECT);
    }
}
