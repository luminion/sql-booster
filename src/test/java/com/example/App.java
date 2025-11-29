package com.example;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.core.env.Environment;

import java.net.InetAddress;

@SpringBootApplication
@Slf4j
public class App {

    public static void main(String[] args) throws Exception {
        ConfigurableApplicationContext application = SpringApplication.run(App.class, args);
        Environment env = application.getEnvironment();
        String host = InetAddress.getLocalHost().getHostAddress();
        String port = env.getProperty("server.port");
        String context = env.getProperty("server.servlet.context-path");
        if (context == null) {
            context = "";
        }
        if (!context.startsWith("/")) {
            context = "/" + context;
        }
        if (!context.endsWith("/")) {
            context = context + "/";
        }
        log.info("Application started swagger at: http://{}:{}{}swagger-ui.html", host, port, context);
        log.info("Application started springdoc at: http://{}:{}{}swagger", host, port, context);
        log.info("Application started smartDoc at: http://{}:{}{}doc/index.html", host, port, context);
        log.info("Application started springdoc at: http://127.0.0.1/swagger");
//        TableMetaRegistry.removeProvider(DefaultTableResolver.class);
    }

}
