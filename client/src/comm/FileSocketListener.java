package comm;

import comm.message.response.file.FileInfo;

/**
 * Created by jimmywest on 2014-12-07.
 */
public interface FileSocketListener {

    public abstract void updateInfo(FileInfo message) ;
    public abstract void updateLine(int lineNumber, String line) ;
    public abstract void addLineAfter(int lineNumber) ;
    public abstract void removeLine(int lineNumber) ;

}
