package comm.message;

/**
 * Created by jimmywest on 2014-12-04.
 */
public enum MessageType {
    CONNECT(1), DISCONNECT(2);

    private final byte code;
    MessageType(int code){this.code = (byte)code;}
    public byte getCode(){return code;}
}
