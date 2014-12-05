package comm.message;

import comm.crypt.Key;

/**
 * Created by jimmywest on 2014-12-04.
 */
public class Connect extends Message {

    public Connect() {
        super(MessageType.CONNECT);
    }

    public void setClientKey(Key clientKey){
        setData(clientKey.getKey());
    }
}
