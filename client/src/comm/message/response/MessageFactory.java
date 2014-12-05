package comm.message.response;

import comm.message.Message;
import comm.message.MessageType;
import comm.message.response.*;
import comm.message.response.Error;

import java.util.Arrays;

/**
 * Created by jimmywest on 2014-12-05.
 */
public class MessageFactory {

    /**
     * Message factory of raw data from a InputStream.
     *
     * @param raw       The raw data byte array from InputStream.
     * @param length    The amount of data in the raw byte array.
     * @return          A Message, or null.
     */
    public static Message createMessage(byte[] raw, int length){
        if (length > 3) {
            byte type = raw[0];
            byte id = raw[1];
            byte size = raw[2];
            if ( size > 0 && length >= size + 3 ) {
                byte[] data = Arrays.copyOfRange(raw, 3, size + 3);
                return factory(type, id, data);
            }else if(size == 0) {
                return factory(type, id);
            }
        }
        return null;
    }

    /**
     * Message factory creator for messages without a data field.
     *
     * @param type  Byte value of the message type.
     * @param id    The message id number.
     * @return      A Message of its corresponding subclass, or null if a message can't be created.
     */
    private static Message factory(int type, byte id) {
        Message message = null;
        switch (type) {
            case 200:
                message = new Ok(id);
                break;
            case 201:
                message = new Ack(id);
                break;
            case 210:
                message = new Accept(id);
                break;
            case 211:
                message = new Reject(id);
                break;
            default:
                message = new Message(id, MessageType.UNKNOWN);
        }
        return message;
    }

    /**
     * Message factory creator for messages with a data field.
     *
     * @param type  Byte value of the message type.
     * @param id    The message id number.
     * @param data  The data field of the message.
     * @return      A Message of its corresponding subclass, or null if a message can't be created.
     */
    private static Message factory(int type, byte id, byte[] data) {
        Message message = null;
        switch (type) {
            case 250:
                message = new Error(id, data);
        }
        return message;
    }
}
