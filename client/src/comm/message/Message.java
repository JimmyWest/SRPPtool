package comm.message;

/**
 * Created by jimmywest on 2014-12-04.
 */
public class Message {

    private MessageType type;
    private static byte counter = 0;
    private byte id;
    private byte length;
    private byte[] data;

    public Message(MessageType type) { this(type,null); }
    public Message(MessageType type, byte[] data) {
        counter();
        this.type = type;
        this.data = data;
    }
    public Message(byte id, MessageType type) {
        this(id,type,new byte[0]);
    }
    public Message(byte id, MessageType type, byte[] data){
        this.id = id;
        this.type = type;
        this.data = data;
    }

    private synchronized void counter() {
        this.id = counter;
        counter++;
    }

    public void setType(MessageType type){
        this.type = type;
    }

    public void setData(byte[] data) {
        // TODO: Add exception on length over 255.
        this.length = (byte)data.length;
        this.data = data;
    }

    public byte getId() {
        return id;
    }

    public byte[] getMessage(){
        int size = ((int)((3 + data.length)/8)+1)*8;
        byte[] msg = new byte[size];
        msg[0] = type.getCode();
        msg[1] = id;
        msg[2] = length;
        System.arraycopy(data,0,msg,3,length);
        return msg;
    }

}
