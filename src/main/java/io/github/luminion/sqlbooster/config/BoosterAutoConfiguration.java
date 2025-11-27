package io.github.luminion.sqlbooster.config;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import io.github.luminion.sqlbooster.extension.mybatisplus.MybatisPlusTableInfoProvider;
import io.github.luminion.sqlbooster.provider.TableInfoProvider;
import io.github.luminion.sqlbooster.provider.support.BasicTableInfoProvider;
import io.github.luminion.sqlbooster.util.BoostUtils;
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
 * Mybatis-Boost 自动配置类.
 *
 * @author luminion
 * @since 1.0.0
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

        Map<String, TableInfoProvider> providerMap = applicationContext.getBeansOfType(TableInfoProvider.class);
        for (TableInfoProvider provider : providerMap.values()) {
            BoostUtils.registerProvider(provider);
        }
        log.debug("{} boost providers registered", providerMap.size());
    }

    @Configuration(proxyBeanMethods = false)
    @ConditionalOnClass(BaseMapper.class)
    static class MybatisPlusConfiguration {

        @Bean
//        @ConditionalOnMissingBean
        public TableInfoProvider mybatisPlusProvider() {
            log.debug("MybatisPlusTableInfoProvider configured");
            return new MybatisPlusTableInfoProvider();
        }
    }

    @Configuration(proxyBeanMethods = false)
    @ConditionalOnClass(SqlSessionFactory.class)
    static class MybatisConfiguration {

        @Bean
//        @ConditionalOnMissingBean
        @ConditionalOnBean(SqlSessionFactory.class)
        public TableInfoProvider mybatisProvider(SqlSessionFactory sqlSessionFactory) {
            boolean mapUnderscoreToCamelCase = sqlSessionFactory.getConfiguration().isMapUnderscoreToCamelCase();
            log.debug("BasicTableInfoProvider for mybatis configured");
            return new BasicTableInfoProvider(mapUnderscoreToCamelCase);
        }
    }
}
