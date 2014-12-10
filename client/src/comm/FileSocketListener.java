package comm;

import file.FileInfo;

/**
 * Created by jimmywest on 2014-12-07.
 */
public interface FileSocketListener {

    public abstract void updateInfo(FileInfo fileinfo) ;
    public abstract void updateLine(int lineNumber, String line) ;
    public abstract void addLine(int lineNumber) ;
    public abstract void removeLine(int lineNumber) ;

}
