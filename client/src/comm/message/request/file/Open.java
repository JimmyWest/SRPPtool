package comm.message.request.file;

import comm.message.Message;
import comm.message.MessageType;

import java.nio.ByteBuffer;

/**
 * Created by jimmywest on 2014-12-05.
 */
public class Open extends Message {

    private int fileId;

    public Open() {
        super(MessageType.FILE_OPEN);
    }

    public Open(int id) {
        super(MessageType.FILE_OPEN);
        setFileId(id);
    }

    public void setFileId(int id) {
        this.fileId = id;
        setData(ByteBuffer.allocate(4).putInt(this.fileId).array());
    }
}
