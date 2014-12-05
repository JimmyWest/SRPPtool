package comm.message;

/**
 * Created by jimmywest on 2014-12-05.
 */
public class Disconnect extends Message{


    public Disconnect() {
        super(MessageType.DISCONNECT,new byte[0]);
    }
}
