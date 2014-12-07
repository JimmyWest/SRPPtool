package comm.message.response.file;

import comm.message.Message;
import comm.message.MessageType;

/**
 * Created by jimmywest on 2014-12-07.
 */
public class LineRemove extends Message {

    private int lineNumber;
    public LineRemove(byte id, byte[] data) {
        super(id, MessageType.FILE_LINE_REMOVE, data);
        for(int i=0;i<4;i++) {
            lineNumber <<= 8;
            lineNumber += data[i];
        }
    }

    public int getLineNumber() {
        return lineNumber;
    }
}
