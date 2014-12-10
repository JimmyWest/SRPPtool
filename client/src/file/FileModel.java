package file;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;

/**
 * Created by jimmywest on 2014-12-07.
 */
public class FileModel {

    private FileInfo fileInfo;
    private ArrayList<String> lines;

    public FileModel(FileInfo fileInfo) {
        this.fileInfo = fileInfo;
        initLines();
    }

    private void initLines() {
        int nol = fileInfo.getNumberOfLines();
        for(int i=0;i<nol;i++){
            lines.add("");
        }
    }

    public void updateFileInfo(FileInfo fileInfo) {
        this.fileInfo = fileInfo;
    }

    public void updateLine(int index, String text) {
        lines.set(index, text);
    }

    public void newLine(int index) {
        lines.add(index, "");
        fileInfo.setNumberOfLines(lines.size());
    }

    public void removeLine(int index) {
        lines.remove(index);
        fileInfo.setNumberOfLines(lines.size());
    }

    public Iterator<String> getIterator() {
        return lines.iterator();
    }

    public String getLine(int index) {
        return lines.get(index);
    }

    @Override
    public String toString() {
        return "FileModel{" +
                "fileInfo=" + fileInfo +
                ", lines=" + lines +
                '}';
    }
}
