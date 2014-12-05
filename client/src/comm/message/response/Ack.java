package comm.message.response;

import comm.message.Message;
import comm.message.MessageType;

/**
 * Created by jimmywest on 2014-12-05.
 */
public class Ack extends Message{


    public Ack(byte id) {
        super(id, MessageType.ACK);
    }
}
