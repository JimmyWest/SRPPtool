package comm.message.response;

import comm.message.Message;
import comm.message.MessageType;

/**
 * Created by jimmywest on 2014-12-05.
 */
public class Accept extends Message {

    public Accept(byte id) {
        super(id, MessageType.ACCEPT);
    }

}
