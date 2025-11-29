package io.github.luminion.sqlbooster.config;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import io.github.luminion.sqlbooster.core.DefaultTableResolver;
import io.github.luminion.sqlbooster.core.TableMetaRegistry;
import io.github.luminion.sqlbooster.core.TableResolver;
import io.github.luminion.sqlbooster.extension.mybatisplus.MpTableResolver;
import io.github.luminion.sqlbooster.util.MapperUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.session.SqlSessionFactory;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Map;

/**
 * sql-booster 的 Spring Boot 自动配置类。
 */
@Slf4j
@AutoConfiguration
public class BoosterAutoConfiguration implements InitializingBean {

    private final ApplicationContext applicationContext;

    public BoosterAutoConfiguration(ApplicationContext applicationContext) {
        this.applicationContext = applicationContext;
    }

    @Override
    public void afterPropertiesSet() {
        // 初始化 SQL 片段
        Map<String, SqlSessionFactory> sqlSessionFactoryMap = applicationContext.getBeansOfType(SqlSessionFactory.class);
        log.info("Found {} SqlSessionFactory bean(s), starting to configure sqlFragments...", sqlSessionFactoryMap.size());
        for (Map.Entry<String, SqlSessionFactory> entry : sqlSessionFactoryMap.entrySet()) {
            String beanName = entry.getKey();
            SqlSessionFactory sqlSessionFactory = entry.getValue();
            boolean success = MapperUtils.initSqlFragment(sqlSessionFactory);
            if (success) {
                log.debug("sqlFragments configured for SqlSessionFactory bean: {}", beanName);
            } else {
                log.error("sqlFragments configuration failed for SqlSessionFactory bean: {}, dynamic sql may not work", beanName);
            }
        }

        // 注册所有找到的 TableResolver
        Map<String, TableResolver> providerMap = applicationContext.getBeansOfType(TableResolver.class);
        for (TableResolver provider : providerMap.values()) {
            TableMetaRegistry.registerProvider(provider);
        }
        log.debug("{} boost providers registered", providerMap.size());
    }

    /**
     * 当检测到 Mybatis-Plus 时，自动配置 MpTableResolver。
     */
    @Configuration(proxyBeanMethods = false)
    @ConditionalOnClass(BaseMapper.class)
    static class MybatisPlusConfiguration {

        @Bean
        public TableResolver mybatisPlusProvider() {
            log.debug("MpTableResolver configured");
            return new MpTableResolver(Integer.MAX_VALUE - 100);
        }
    }

    /**
     * 当检测到原生 Mybatis 时，自动配置 DefaultTableResolver。
     */
    @Configuration(proxyBeanMethods = false)
    @ConditionalOnClass(SqlSessionFactory.class)
    static class MybatisConfiguration {

        @Bean
        @ConditionalOnBean(SqlSessionFactory.class)
        public TableResolver mybatisProvider(SqlSessionFactory sqlSessionFactory) {
            boolean mapUnderscoreToCamelCase = sqlSessionFactory.getConfiguration().isMapUnderscoreToCamelCase();
            log.debug("DefaultTableResolver for mybatis configured");
            return new DefaultTableResolver(mapUnderscoreToCamelCase, Integer.MAX_VALUE);
        }
    }
}
