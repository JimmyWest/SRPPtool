package comm.message;

/**
 * Created by jimmywest on 2014-12-04.
 */
public class Message {

    private MessageType type;
    private byte length;
    private byte[] data;

    public Message(MessageType type) {
        this.type = type;
    }
    public Message(MessageType type, byte[] data) {
        this.type = type;
        this.data = data;
    }

    public void setType(MessageType type){
        this.type = type;
    }

    public void setData(byte[] data) {
        // TODO: Add exeption on length over 255.
        this.length = (byte)data.length;
        this.data = data;
    }

    public byte[] getMessage(){
        int size = ((int)((2 + data.length)/8)+1)*8;
        byte[] msg = new byte[size];
        msg[0] = type.getCode();
        msg[1] = length;
        System.arraycopy(data,0,msg,2,length);
        return msg;
    }

}
