package com.example;

import io.github.luminion.sqlbooster.model.SqlContext;
import io.github.luminion.sqlbooster.util.ReflectUtils;
import org.junit.jupiter.api.Test;

import java.util.Map;

/**
 * @author luminion
 * @since 1.0.0
 */
public class Test1 {
    
    @Test
    void test1(){
        Map<String, Object> stringObjectMap = ReflectUtils.beanToMap(new SqlContext<>());
        System.out.println(stringObjectMap);
    }
}
