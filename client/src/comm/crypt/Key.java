package comm.crypt;

import java.util.Arrays;

/**
 * Created by jimmywest on 2014-12-04.
 */
public class Key {

    public static int KEY_LENGTH = 8;
    private byte[] key = new byte[KEY_LENGTH];

    public Key(String passPhrase){
        this.key = generateKey(passPhrase);
    }

    public Key(byte[] key){
        this.key = key;
    }

    private byte[] generateKey(String passPhrase){
        byte[] phrase = passPhrase.getBytes();
        int phraseHash = phraseHash(phrase, KEY_LENGTH);
        return generateKey(phrase, phraseHash);
    }

    private byte[] generateKey(byte[] phrase, int hash){
        byte[] key = new byte[KEY_LENGTH];
        int p = 0;
        for (int l = 0; l < key.length; l++) {
            if(p == phrase.length){
                hash = phraseHash(phrase,hash);
                p = 0;
            }
            int ival = (phrase[p]*hash % 255);
            hash += phrase[p];
            byte val = (byte) ival;
            key[l] = val;
            p++;
        }
        return key;
    }

    private int phraseHash(byte[] bytes, int num) {
        for (int i = 0; i < bytes.length; i++) {
            num += bytes[i];
        }
        return num;
    }

    public byte[] getKey() {
        return key;
    }

    @Override
    public String toString() {

        return "Key{" +
                "key=" + Arrays.toString(key) +
                '}';
    }
}
