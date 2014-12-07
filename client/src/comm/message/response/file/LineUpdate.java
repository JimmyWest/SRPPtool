package comm.message.response.file;

import comm.message.Message;
import comm.message.MessageType;

import java.util.Arrays;

/**
 * Created by jimmywest on 2014-12-07.
 */
public class LineUpdate extends Message{

    private int lineNumber = 0;
    private String line;

    public LineUpdate(byte id, byte[] data) {
        super(id, MessageType.FILE_LINE_UPDATE, data);
        for(int i=0;i<4;i++){
            this.lineNumber <<= 8;
            this.lineNumber += data[i];
        }
        byte[] line = Arrays.copyOfRange(data, 4, data.length);
        this.line = new String(line);
    }

    public int getLineNumber() {
        return lineNumber;
    }

    public String getLine() {
        return line;
    }
}
