package comm;

import comm.message.Message;
import comm.message.request.file.Open;
import comm.message.response.file.*;

import java.io.IOException;

/**
 * Created by jimmywest on 2014-12-04.
 */
public class FileSocketHandler extends SocketHandler {

    private FileSocketListener fileSocketListener;

    public FileSocketHandler(String host, int port, String serverPhrase, String clientPhrase, int fileId) throws IOException {
        super(host, port, serverPhrase, clientPhrase);
        openFile(fileId);
    }

    public void setFileSocketListener(FileSocketListener fileSocketListener) {
        this.fileSocketListener = fileSocketListener;
    }

    public void openFile(int fileId) {
        Open openFile = new Open(fileId);
        try {
            send(openFile);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @Override
    protected void handleMessage(Message message) {
        System.out.println("Got message" + message);
        switch (message.getType()) {
            case FILE_KEY:
                handleFileKeyMessage((FileKey) message);
                break;
            case FILE_INFO:
                handleFileInfoMessage((FileInfo) message);
                break;
            case FILE_LINE_UPDATE:
                handleLineUpdateMessage((LineUpdate) message);
                break;
            case FILE_LINE_NEW:
                handleLineNew((LineNew) message);
                break;
            case FILE_LINE_REMOVE:
                handleLineRemoveMessage((LineRemove) message);
        }
    }

    private void handleFileKeyMessage(FileKey message) {
        setFileKey(message.getFileKey());
    }

    private void handleFileInfoMessage(FileInfo message){
        if (fileSocketListener != null)
            fileSocketListener.updateInfo(message);
    }

    private void handleLineUpdateMessage(LineUpdate message) {
        if (fileSocketListener != null)
            fileSocketListener.updateLine(message.getLineNumber(), message.getLine());
    }

    private void handleLineNew(LineNew message) {
        if (fileSocketListener != null)
            fileSocketListener.addLineAfter(message.getPos());
    }

    private void handleLineRemoveMessage(LineRemove message) {
        if (fileSocketListener != null)
            fileSocketListener.removeLine(message.getLineNumber());
    }

}
