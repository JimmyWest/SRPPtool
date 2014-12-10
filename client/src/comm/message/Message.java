package comm.message;

import java.util.Arrays;

/**
 * Created by jimmywest on 2014-12-04.
 */
public class Message {

    private MessageType type;
    private static byte counter = 0;
    private byte id;
    private byte length;
    private byte[] data;
    private int readPointer = 0;

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

    public MessageType getType() {
        return type;
    }

    public void setData(byte[] data) {
        // TODO: Add exception on length over 255.
        this.length = (byte)data.length;
        this.data = data;
    }

    public byte getId() {
        return id;
    }

    public byte[] getPlainText(){
        int size = ((int)((3 + data.length)/8)+1)*8;
        byte[] msg = new byte[size];
        msg[0] = type.getCode();
        msg[1] = id;
        msg[2] = length;
        System.arraycopy(data,0,msg,3,length);
        return msg;
    }

    protected int readByte() {
        byte b = data[readPointer];
        readPointer++;
        return b;
    }

    protected byte[] readBytes(int length) {
        byte[] bytes = Arrays.copyOfRange(data,readPointer,readPointer + length);
        readPointer += length;
        return bytes;
    }

    protected int readInt(){
        int num = 0;
        for(int i=0; i<4; i++) {
            num <<= 8;
            num += data[readPointer];
            readPointer++;
        }
        return num;
    }

    protected String readString() {
        byte[] bytes = Arrays.copyOfRange(data, readPointer, data.length);
        readPointer = data.length;
        return new String(bytes);
    }

    protected String readString(int length) {
        byte[] bytes = Arrays.copyOfRange(data,readPointer,readPointer + length);
        readPointer += length;
        return new String(bytes);
    }

    @Override
    public String toString() {
        return "Message{" +
                "type=" + type +
                ", id=" + id +
                ", length=" + length +
                ", data=" + Arrays.toString(data) +
                '}';
    }

}
