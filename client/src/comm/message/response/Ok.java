package comm.message.response;

import comm.message.Message;
import comm.message.MessageType;

/**
 * Created by jimmywest on 2014-12-05.
 */
public class Ok extends Message{

    public Ok(byte id) {
        super(id, MessageType.OK);
    }
}
