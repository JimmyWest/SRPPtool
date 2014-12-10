package comm.message.response.file;

import comm.message.Message;
import comm.message.MessageType;

/**
 * Created by jimmywest on 2014-12-07.
 */
public class FileInfo extends Message {

    private file.FileInfo fileInfo;

    public FileInfo(byte id, byte[] data) {
        super(id, MessageType.FILE_INFO, data);
        int numberOfLines = readInt();
        String filename = readString();
        fileInfo = new file.FileInfo(filename, numberOfLines);
    }

    public file.FileInfo getFileInfo() {
        return fileInfo;
    }
}
