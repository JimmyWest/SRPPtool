package comm.message.response.file;

import comm.message.Message;
import comm.message.MessageType;

/**
 * Created by jimmywest on 2014-12-07.
 */
public class LineNew extends Message {

    private int lineNumber = 0;

    public LineNew(byte id, byte[] data) {
        super(id, MessageType.FILE_LINE_NEW, data);
        lineNumber = readInt();
    }

    public int getLineNumber() {
        return lineNumber;
    }

}
