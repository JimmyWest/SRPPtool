package comm.crypt;

import comm.message.Message;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.util.Arrays;

/**
 * Created by jimmywest on 2014-12-04.
 */
public class Crypt  extends KeyRing{

    public Crypt (String serverPhrase){
        super();
        setServerKey(serverPhrase);
    }

    public KeyRing getKeyRing() {
        return this;
    }

    public byte[] encrypt(byte[] plainText) {
        try {
            Cipher cipher = getCipher();
            cipher.init(Cipher.ENCRYPT_MODE, getSecretKey(), getIVec());
            byte[] cipherText = cipher.doFinal(plainText);
            System.out.println("Len:"+plainText.length+", Msg: "+ Arrays.toString(plainText));
            System.out.println("Len:"+cipherText.length+", CM : "+Arrays.toString(cipherText));
            return cipherText;
        } catch (InvalidKeyException e) {
            e.printStackTrace();
        } catch (InvalidAlgorithmParameterException e) {
            e.printStackTrace();
        } catch (BadPaddingException e) {
            e.printStackTrace();
        } catch (IllegalBlockSizeException e) {
            e.printStackTrace();
        }
        // TODO: Add throw exception
        return new byte[0];
    }

    public byte[] decrypt(byte[] cipherText) {
        try {
            Cipher cipher = getCipher();
            cipher.init(Cipher.DECRYPT_MODE, getSecretKey(), getIVec());
            byte[] plainText = cipher.doFinal(cipherText);
            return plainText;
        } catch (InvalidKeyException e) {
            e.printStackTrace();
        } catch (InvalidAlgorithmParameterException e) {
            e.printStackTrace();
        } catch (BadPaddingException e) {
            e.printStackTrace();
        } catch (IllegalBlockSizeException e) {
            e.printStackTrace();
        }
        // TODO: Add throw exception
        return new byte[0];
    }

}
