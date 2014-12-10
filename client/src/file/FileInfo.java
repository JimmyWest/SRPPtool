package file;

import java.nio.ByteBuffer;
import java.util.Arrays;

/**
 * Created by jimmywest on 2014-12-10.
 */
public class FileInfo {

    private String filename;
    private int numberOfLines;

    public FileInfo(String filename, int numberOfLines) {
        this.filename = filename;
        this.numberOfLines = numberOfLines;
    }

    public String getFilename() {
        return filename;
    }

    public void setFilename(String filename) {
        this.filename = filename;
    }

    public int getNumberOfLines() {
        return numberOfLines;
    }

    public void setNumberOfLines(int numberOfLines) {
        this.numberOfLines = numberOfLines;
    }
}
