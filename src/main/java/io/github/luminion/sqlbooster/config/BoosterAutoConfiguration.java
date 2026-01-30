package io.github.luminion.sqlbooster.config;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import io.github.luminion.sqlbooster.core.DefaultTableResolver;
import io.github.luminion.sqlbooster.core.TableMetaRegistry;
import io.github.luminion.sqlbooster.core.TableResolver;
import io.github.luminion.sqlbooster.extension.mybatis.BoosterMapperUtils;
import io.github.luminion.sqlbooster.extension.mybatisplus.MpTableResolver;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.session.SqlSessionFactory;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingClass;
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
        Map<String, SqlSessionFactory> sqlSessionFactoryMap = applicationContext
                .getBeansOfType(SqlSessionFactory.class);
        log.info("Found {} SqlSessionFactory bean(s), starting to configure sqlFragments...",
                sqlSessionFactoryMap.size());
        for (Map.Entry<String, SqlSessionFactory> entry : sqlSessionFactoryMap.entrySet()) {
            String beanName = entry.getKey();
            SqlSessionFactory sqlSessionFactory = entry.getValue();
            boolean success = BoosterMapperUtils.initSqlFragment(sqlSessionFactory);
            if (success) {
                log.debug("sqlFragments configured for SqlSessionFactory bean: {}", beanName);
            } else {
                log.error("sqlFragments configuration failed for SqlSessionFactory bean: {}, dynamic sql may not work",
                        beanName);
            }
        }
        TableResolver resolver = applicationContext.getBean(TableResolver.class);
        TableMetaRegistry.register(resolver);
    }

    @Configuration(proxyBeanMethods = false)
    @ConditionalOnClass({BaseMapper.class, SqlSessionFactory.class})
    static class MyBatisPlusConfiguration {
        @Bean
        @ConditionalOnMissingBean(TableResolver.class)
        public TableResolver tableResolver() {
            log.info("Using MpTableResolver");
            return new MpTableResolver();
        }
    }

    @Configuration(proxyBeanMethods = false)
    @ConditionalOnClass(SqlSessionFactory.class)
    @ConditionalOnMissingClass("com.baomidou.mybatisplus.core.mapper.BaseMapper")
    static class MyBatisConfiguration {

        @Bean
        @ConditionalOnMissingBean(TableResolver.class)
        public TableResolver tableResolver(ObjectProvider<SqlSessionFactory> factoryProvider) {
            boolean camelCase = true;
            SqlSessionFactory factory = factoryProvider.stream().findFirst().orElse(null);
            if (factory != null) {
                try {
                    camelCase = factory.getConfiguration().isMapUnderscoreToCamelCase();
                } catch (Exception ignored) {
                    
                }
            }
            return new DefaultTableResolver(camelCase);
        }
    }

}
