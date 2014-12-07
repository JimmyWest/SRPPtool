package comm.message;

/**
 * Created by jimmywest on 2014-12-04.
 */
public enum MessageType {

    // Connection request message
    CONNECT(1), DISCONNECT(2),

    // Main request messages
    FOLDER(10),

    // File request message
    FILE_OPEN(100), FILE_SUBSCRIBE(101), FILE_INFO(110),FILE_LINE_GET(111),
    FILE_LINE_NEW(120),FILE_LINE_REMOVE(121),FILE_LINE_UPDATE(122),
    FILE_CURSOR_POS(150),

    // Response message types
    OK(200), ACK(201), ACCEPT(210),
    FILE_KEY(220),
    ERROR(250), REJECT(251),

    // Unknown Type
    UNKNOWN(0);

    private final byte code;
    MessageType(int code){this.code = (byte)code;}
    public byte getCode(){return code;}
}
