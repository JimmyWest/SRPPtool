package file;

import comm.FileSocketHandler;
import comm.FileSocketListener;

/**
 * Created by jimmywest on 2014-12-07.
 */
public class FileController implements FileSocketListener{


    private FileSocketHandler fileSocketHandler = null;
    private int fileId;
    private FileModel filemodel;

    public FileController(int fileId) {
        this.fileId = fileId;
    }

    public void setFileSocketHandler(FileSocketHandler fileSocketHandler) {
        this.fileSocketHandler = fileSocketHandler;
        this.fileSocketHandler.setFileSocketListener(this);
        this.fileSocketHandler.openFile(fileId);
        this.fileSocketHandler.start();
    }

    @Override
    public void updateInfo(FileInfo fileInfo) {
        filemodel = new FileModel(fileInfo);
    }

    @Override
    public void updateLine(int index, String text) {
        filemodel.updateLine(index, text);
    }

    @Override
    public void addLine(int index) {
        filemodel.newLine(index);
    }

    @Override
    public void removeLine(int index) {
        filemodel.removeLine(index);
    }
}
