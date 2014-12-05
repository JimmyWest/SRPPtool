package comm.crypt;

import javax.crypto.Cipher;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import java.security.NoSuchAlgorithmException;

import static comm.crypt.KeyRing.Mode.*;

/**
 * Created by jimmywest on 2014-12-04.
 */
public class KeyRing {


    public enum Mode {
        NONE,S,SCS,SCF
    }

    private Mode mode;
    private IvParameterSpec iVec = new IvParameterSpec((new Key("Major version 0").getKey()));
    private Key serverKey = null;
    private Key clientKey = null;
    private Key fileKey = null;

    public KeyRing(){
        updateMode();
    }

    private void updateMode() {
        this.mode = SCF;
        if (fileKey == null) this.mode = SCS;
        if (clientKey == null) this.mode = S;
        if (serverKey == null) this.mode = NONE;
    }

    public IvParameterSpec getIVec() {
        return iVec;
    }

    public void setServerKey(String phrase) {
        this.serverKey = new Key(phrase);
        updateMode();
    }

    public void setServerKey(byte[] bytes) {
        this.serverKey = new Key(bytes);
        updateMode();
    }

    public void setServerKey(Key key) {
        this.serverKey = key;
        updateMode();
    }

    public Key getServerKey() {
        return serverKey;
    }

    public void setClientKey(String phrase){
        this.clientKey = new Key(phrase);
        updateMode();
    }

    public void setClinetKey(byte[] bytes) {
        this.clientKey = new Key(bytes);
        updateMode();
    }

    public void setClientKey(Key key) {
        this.clientKey = key;
        updateMode();
    }

    public Key getClientKey() {
        return clientKey;
    }

    public void setFileKey(String phrase){
        this.fileKey = new Key(phrase);
        updateMode();
    }

    public void setFileKey(byte[] bytes) {
        this.fileKey = new Key(bytes);
        updateMode();
    }

    public void setFileKey(Key key) {
        this.fileKey = key;
        updateMode();
    }

    public Key getFileKey() {
        return fileKey;
    }

    public byte[] getKey(){
        Key[] keys = getKeys();
        byte[] key = new byte[0];
        for (int i = 0; i < keys.length; i++) {
            byte[] tmp = key;
            int offset = tmp.length;
            int length = keys[i].getKey().length;
            key = new byte[offset+length];
            System.arraycopy(tmp,0,key,0,offset);
            System.arraycopy(keys[i].getKey(),0,key,offset,length);
        }
        return key;
    }

    public Key[] getKeys() {
        Key[] key = new Key[0];
        switch (mode) {
            case NONE: break;
            case S:
                key = new Key[1];
                key[0] = serverKey;
                break;
            case SCS:
                key = new Key[3];
                key[0] = serverKey;
                key[1] = clientKey;
                key[2] = serverKey;
                break;
            case SCF:
                key = new Key[3];
                key[0] = serverKey;
                key[1] = clientKey;
                key[3] = fileKey;
        }
        return key;
    }


    public SecretKey getSecretKey() {
        byte[] key = getKey();
        switch (mode) {
            case S: return new SecretKeySpec(key,"DES");
            case SCS: return new SecretKeySpec(key,"DESede");
            case SCF: return new SecretKeySpec(key, "DESede");
        }
        return null;
    }

    public Cipher getCipher() {
        try {
            switch (mode) {
                case S: return Cipher.getInstance("DES/CBC/NoPadding");
                case SCS: return Cipher.getInstance("DESede/CBC/NoPadding");
                case SCF: return Cipher.getInstance("DESede/CBC/NoPadding");
            }
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        } catch (NoSuchPaddingException e) {
            e.printStackTrace();
        }
        return null;
    }

}