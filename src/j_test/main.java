package j_test;

class JSONException extends Exception {
}

interface CustomCallback {
    public void completionHandler(Boolean success, String result) throws JSONException, InterruptedException;
}

class HttpGetRequest {
    public HttpGetRequest(CustomCallback cb) {

    }
}

@FunctionalInterface
interface HTTPRequestHandler {
    public void completionHandler(Boolean success, String result) throws JSONException, InterruptedException;
}

public class main {
    static HttpGetRequest newHTTPRequest(HTTPRequestHandler f) {
        return new HttpGetRequest(new CustomCallback() {
            public void completionHandler(Boolean success, String result) throws JSONException, InterruptedException {
                f.completionHandler(success, result);
            }
        });
    }

    public static void main(String[] args) {
        HttpGetRequest get = newHTTPRequest((success, result) -> {
            System.out.println(String.valueOf(success) + " " + result);
        });
    }
}

