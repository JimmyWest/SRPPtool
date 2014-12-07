package comm.message.response.file;

import comm.crypt.Key;
import comm.message.Message;
import comm.message.MessageType;

/**
 * Created by jimmywest on 2014-12-07.
 */
public class FileKey extends Message{

    private Key fileKey;

    public FileKey(byte id, byte[] key) {
        super(id, MessageType.FILE_KEY);
        this.fileKey = new Key(key);
    }

    public Key getFileKey() {
        return fileKey;
    }
}
