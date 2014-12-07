package comm.message.response.file;

import comm.message.Message;
import comm.message.MessageType;

/**
 * Created by jimmywest on 2014-12-07.
 */
public class LineNew extends Message {

    private int after = 0;

    public LineNew(byte id, byte[] data) {
        super(id, MessageType.FILE_LINE_NEW, data);
        for(int i=0;i<4;i++) {
            after <<= 8;
            after += data[i];
        }
    }

    public int getPos() {
        return after;
    }
}
