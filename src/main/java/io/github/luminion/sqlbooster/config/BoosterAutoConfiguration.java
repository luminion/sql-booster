package io.github.luminion.sqlbooster.config;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import io.github.luminion.sqlbooster.core.Booster;
import io.github.luminion.sqlbooster.extension.mybatis.BoosterMapper;
import io.github.luminion.sqlbooster.extension.mybatis.DefaultTableResolver;
import io.github.luminion.sqlbooster.extension.mybatisplus.MpTableResolver;
import io.github.luminion.sqlbooster.metadata.BoosterRegistry;
import io.github.luminion.sqlbooster.metadata.TableMetaRegistry;
import io.github.luminion.sqlbooster.metadata.TableResolver;
import io.github.luminion.sqlbooster.util.BoosterMapperUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.session.SqlSessionFactory;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * sql-booster 的 Spring Boot 自动配置类。
 */
@Slf4j
@AutoConfiguration
public class BoosterAutoConfiguration implements InitializingBean, DisposableBean {

    private final ApplicationContext applicationContext;
    private final List<TableResolver> registeredTableResolvers = new ArrayList<>();
    private final List<Booster<?, ?>> registeredBoosters = new ArrayList<>();
    private volatile boolean initialized;

    public BoosterAutoConfiguration(ApplicationContext applicationContext) {
        this.applicationContext = applicationContext;
    }

    @Override
    public synchronized void afterPropertiesSet() {
        if (initialized) {
            return;
        }
        Map<String, SqlSessionFactory> sqlSessionFactoryMap = applicationContext.getBeansOfType(SqlSessionFactory.class);
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

        try {
            Map<String, TableResolver> providerMap = applicationContext.getBeansOfType(TableResolver.class);
            registerTableResolvers(providerMap.values());
            log.debug("{} TableResolver registered", providerMap.size());

            Map<String, BoosterMapper> mapperMap = applicationContext.getBeansOfType(BoosterMapper.class);
            for (Map.Entry<String, BoosterMapper> entry : mapperMap.entrySet()) {
                BoosterRegistry.registerBooster(entry.getKey(), entry.getValue());
                registeredBoosters.add(entry.getValue());
            }
            log.debug("{} BoosterMapper registered", mapperMap.size());
            initialized = true;
        } catch (RuntimeException e) {
            destroy();
            throw e;
        }
    }

    @Override
    public synchronized void destroy() {
        for (Booster<?, ?> booster : registeredBoosters) {
            BoosterRegistry.removeBooster(booster);
        }
        registeredBoosters.clear();

        for (TableResolver resolver : registeredTableResolvers) {
            TableMetaRegistry.removeTableResolver(resolver);
        }
        registeredTableResolvers.clear();
        initialized = false;
    }

    private void registerTableResolvers(Collection<TableResolver> resolvers) {
        for (TableResolver resolver : resolvers) {
            if (resolver != null && TableMetaRegistry.addTableResolver(resolver)) {
                registeredTableResolvers.add(resolver);
            }
        }
    }

    @Configuration(proxyBeanMethods = false)
    @ConditionalOnClass(BaseMapper.class)
    static class MybatisPlusConfiguration {

        @Bean
        public TableResolver mybatisPlusProvider() {
            log.debug("MpTableResolver configured");
            return new MpTableResolver(Integer.MAX_VALUE - 100);
        }
    }

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