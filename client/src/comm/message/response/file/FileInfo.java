package comm.message.response.file;

import comm.message.Message;
import comm.message.MessageType;

import java.util.Arrays;

/**
 * Created by jimmywest on 2014-12-07.
 */
public class FileInfo extends Message {

    private String filename;

    private int numberOfLines = 0;

    public FileInfo(byte id, byte[] data) {
        super(id, MessageType.FILE_INFO, data);
        for(int i=0; i<4; i++) {
            this.numberOfLines <<= 8;
            this.numberOfLines += data[i];
        }
        byte[] name = Arrays.copyOfRange(data, 4, data.length);
        filename = new String(name);
    }

    public String getFilename(){
        return filename;
    }

    public int getNumberOfLines() {
        return numberOfLines;
    }

}
