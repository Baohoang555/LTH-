import Network.Socket

main :: IO ()
main = do
    -- Bước 1: Tạo socket
    sock <- socket AF_INET Stream defaultProtocol

    -- Bước 2: Kết nối tới server
    let serverAddr = SockAddrInet 8080 (tupleToHostAddress (127,0,0,1)) -- Kết nối tới localhost:8080
    connect sock serverAddr

    putStrLn "Connected to server"

    -- Bước 3: Giao tiếp
    sendAll sock "Xin chào server, đây là nước đi của tôi" -- Gửi dữ liệu
    response <- recv sock 1024 -- Nhận phản hồi
    print response

    -- Bước 4: Đóng kết nối
    close sock