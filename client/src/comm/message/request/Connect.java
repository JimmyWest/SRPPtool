package comm.message.request;

import comm.crypt.Key;
import comm.message.Message;
import comm.message.MessageType;

/**
 * Created by jimmywest on 2014-12-04.
 */
public class Connect extends Message {

    private Key clientKey;

    public Connect() {
        super(MessageType.CONNECT);
    }


    public void setClientKey(String clientPhrase) {
        this.clientKey = new Key(clientPhrase);
        setData(clientKey.getKey());
    }

    public void setClientKey(Key clientKey){
        this.clientKey = clientKey;
        setData(clientKey.getKey());
    }

    public Key getClientKey() {
        return clientKey;
    }

}
