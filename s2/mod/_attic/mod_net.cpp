#include <thread>
#include <sys/socket.h>
#include <string.h>
#include <netdb.h>
#include <unistd.h>
#include <iostream>
#include <queue>

#include "../cwal/s2/s2_amalgamation.h"

#define MAX_THREADS 8
#define MAX_READ_SIZE 65536

static volatile bool Running = false;
static volatile uint16_t Port = 8080;
static volatile int RequestCount = 0;
static volatile int listener;

struct Request {
    int id;
    std::string headers;
    std::string post;
    cwal_int_t remainingData;
};
static std::queue<Request *> RequestQueue;

static int Start(cwal_callback_args const *args, cwal_value **rv) {
    *rv = args->self;
    if (Running) return 0;

    // Read the First Argument
    Port = (uint16_t) cwal_value_get_integer(args->argv[0]);

    // Allocate a Socket
    listener = socket(AF_INET, SOCK_STREAM, 0);
    if (listener == -1) {
        *rv = cwal_value_null();
        return 0;
    }

    // Bind to a Port
    struct sockaddr_in servaddr;
    bzero(&servaddr, sizeof(servaddr));
    servaddr.sin_family = AF_INET;
    servaddr.sin_addr.s_addr = htons(INADDR_ANY);
    servaddr.sin_port = htons(Port);
    if (bind(listener, (struct sockaddr *) &servaddr, sizeof(servaddr)) == -1) {
        *rv = cwal_value_null();
        return 0;
    }

    // Accept Connections on the Socket
    if (listen(listener, 10) == -1) {
        *rv = cwal_value_null();
        return 0;
    }

    // Start the Listener Threads
    Running = true;
    new std::thread([](){
        do {
            if (RequestCount >= MAX_THREADS) continue;

            // Start a New Listener
            new std::thread([&](){
                int comm_fd = accept(listener, NULL, NULL);
                if (comm_fd == -1) {
                    --RequestCount;
                    return;
                }

                // Build the Request
                Request *request = new Request();
                request->id = comm_fd;

                // Read Request Headers
                char str[MAX_READ_SIZE];
                cwal_int_t totalRead = 0;
                const char *EOH, *BOCLH, *EOCLH;
                ssize_t n;
                while ((n = read(comm_fd, str, MAX_READ_SIZE)) > 0) {
                    totalRead += n;
                    request->headers += std::string(str, (unsigned long) n);
                    if ((EOH = strstr(request->headers.c_str(), "\r\n\r\n"))) {
                        unsigned long headersLength = EOH - request->headers.c_str();
                        if ((BOCLH = strstr(request->headers.c_str(), "\r\nContent-Length:")) && (EOCLH = strstr(BOCLH + 18, "\r\n"))) {
                            std::string strContentLength = std::string(BOCLH + 18, EOCLH - BOCLH - 18);
                            cwal_int_t contentLength = std::stol(strContentLength);
                            if (totalRead > headersLength+4) {
                                request->post = std::string(EOH+4, totalRead - (headersLength+4));
                            }
                            request->remainingData = contentLength - request->post.length();
                        }
                        request->headers = request->headers.substr(0, headersLength);
                        break;
                    }
                }

                // Add the Request to the Queue
                RequestQueue.push(request);
            });

            // Increment the Counter
            ++RequestCount;
        } while (Running);
    });

    // Success!
    return 0;
}

static int Open(cwal_callback_args const *args, cwal_value **rv) {
    if (!Running) {
        *rv = cwal_value_false();
        return 0;
    }

    if (RequestQueue.empty()) {
        *rv = cwal_value_true();
        return 0;
    }

    Request *request = RequestQueue.front();
    RequestQueue.pop();
    cwal_object *ret = cwal_new_object(args->engine);
    cwal_object_set_s(ret, cwal_new_string(args->engine, "id", 0), cwal_new_integer(args->engine, request->id));
    cwal_object_set_s(ret, cwal_new_string(args->engine, "headers", 0), cwal_string_value(cwal_new_string(args->engine, request->headers.c_str(), request->headers.size())));
    cwal_object_set_s(ret, cwal_new_string(args->engine, "post", 0), cwal_string_value(cwal_new_string(args->engine, request->post.c_str(), request->post.size())));
    cwal_object_set_s(ret, cwal_new_string(args->engine, "remainingData", 0), cwal_new_integer(args->engine, request->remainingData));
    delete request;

    *rv = cwal_object_value(ret);
    return 0;
}

static int Read(cwal_callback_args const *args, cwal_value **rv) {
    *rv = args->argv[0];
    if (!Running) return 0;

    // Read the Request Object
    cwal_object *obj = cwal_value_get_object(args->argv[0]);
    cwal_int_t remainingData = cwal_value_get_integer(cwal_object_get(obj, "remainingData", 0));
    if (remainingData <= 0) return 0;
    int id = (int) cwal_value_get_integer(cwal_object_get(obj, "id", 0));
    cwal_size_t postLen;
    std::string post = std::string(cwal_value_get_cstr(cwal_object_get(obj, "post", 0), &postLen), postLen);

    // Read the Remaining Data
    char str[MAX_READ_SIZE];
    ssize_t n;
    while ((n = read(id, str, MAX_READ_SIZE > remainingData ? (size_t) remainingData : MAX_READ_SIZE)) > 0) {
        remainingData -= n;
        post += std::string(str, (unsigned long) n);
        if (remainingData == 0) break;
    }

    // Return the Completed Request
    cwal_object_set_s(obj, cwal_new_string(args->engine, "post", 0), cwal_string_value(cwal_new_string(args->engine, post.c_str(), post.size())));
    cwal_object_set_s(obj, cwal_new_string(args->engine, "remainingData", 0), cwal_new_integer(args->engine, 0));
    return 0;
}

static int Close(cwal_callback_args const *args, cwal_value **rv) {
    if (!Running) {
        *rv = cwal_value_null();
        return 0;
    }

    // Send the Response
    int id = (int) cwal_value_get_integer(args->argv[0]);
    cwal_size_t bodyLen = 0;
    const char *body = cwal_value_get_cstr(args->argv[1], &bodyLen);

    // Response
    write(id, body, bodyLen);

    // Close
    close(id);

    // Decrement the Counter
    --RequestCount;

    *rv = cwal_value_true();
    return 0;
}

static int Stop(cwal_callback_args const *args, cwal_value **rv) {
    if (Running) {
        Running = false;

        // Close Listeners
        while (Request *request = RequestQueue.front()) {
            RequestQueue.pop();
            close(request->id);
        }

        // Close Socket
        close(listener);
    }

    *rv = args->self;
    return 0;
}

static int Init(s2_engine *se, cwal_value *ns) {
	// Return the Regex Functions
	s2_func_def const funcs[] = {
		S2_FUNC2("Start", Start),
        S2_FUNC2("Open", Open),
        S2_FUNC2("Read", Read),
        S2_FUNC2("Close", Close),
        S2_FUNC2("Stop", Stop),
		s2_func_def_empty_m
	};
	s2_install_functions(se, ns, funcs, 0);
	return 0;
}

S2_MODULE_IMPL(net, Init);
S2_MODULE_IMPL1(net, Init);
