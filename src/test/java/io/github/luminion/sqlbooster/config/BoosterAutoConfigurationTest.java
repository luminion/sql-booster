package io.github.luminion.sqlbooster.config;

import io.github.luminion.sqlbooster.extension.mybatis.BoosterMapper;
import org.apache.ibatis.session.Configuration;
import org.apache.ibatis.session.SqlSessionFactory;
import org.apache.ibatis.session.defaults.DefaultSqlSessionFactory;
import io.github.luminion.sqlbooster.metadata.BoosterRegistry;
import io.github.luminion.sqlbooster.metadata.TableMeta;
import io.github.luminion.sqlbooster.metadata.TableMetaRegistry;
import io.github.luminion.sqlbooster.metadata.TableResolver;
import io.github.luminion.sqlbooster.model.SqlContext;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.springframework.context.support.GenericApplicationContext;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.List;

public class BoosterAutoConfigurationTest {

    @Before
    @After
    public void resetRegistries() {
        BoosterRegistry.clear();
        for (TableResolver resolver : TableMetaRegistry.checkoutTableResolver()) {
            TableMetaRegistry.removeTableResolver(resolver);
        }
    }

    @Test
    public void shouldSupportMultipleSqlSessionFactories() {
        Configuration firstConfiguration = new Configuration();
        firstConfiguration.setMapUnderscoreToCamelCase(true);
        Configuration secondConfiguration = new Configuration();
        secondConfiguration.setMapUnderscoreToCamelCase(false);
        Map<String, SqlSessionFactory> factories = new LinkedHashMap<>();
        factories.put("z-second", new DefaultSqlSessionFactory(secondConfiguration));
        factories.put("a-first", new DefaultSqlSessionFactory(firstConfiguration));

        TableResolver resolver = new BoosterAutoConfiguration.MybatisConfiguration().mybatisProvider(factories);

        Assert.assertEquals("a.user_name", resolver.resolve(TestEntity.class)
                .getPropertyToColumnAliasMap().get("userName"));
    }
    @Test
    public void shouldRemoveRegisteredResolversAndBoostersOnDestroy() throws Exception {
        GenericApplicationContext applicationContext = new GenericApplicationContext();
        TestTableResolver resolver = new TestTableResolver();
        TestBoosterMapper mapper = new TestBoosterMapper();
        applicationContext.getBeanFactory().registerSingleton("testResolver", resolver);
        applicationContext.getBeanFactory().registerSingleton("testMapper", mapper);
        applicationContext.refresh();

        BoosterAutoConfiguration autoConfiguration = new BoosterAutoConfiguration(applicationContext);
        autoConfiguration.afterPropertiesSet();

        Assert.assertTrue(TableMetaRegistry.checkoutTableResolver().contains(resolver));
        Assert.assertSame(mapper, BoosterRegistry.getRequiredBooster(TestEntity.class, TestView.class));

        autoConfiguration.destroy();

        Assert.assertFalse(TableMetaRegistry.checkoutTableResolver().contains(resolver));
        try {
            BoosterRegistry.getRequiredBooster(TestEntity.class, TestView.class);
            Assert.fail("Expected no registered booster after destroy.");
        } catch (IllegalStateException expected) {
            Assert.assertTrue(expected.getMessage().contains(TestEntity.class.getName()));
        } finally {
            applicationContext.close();
        }
    }

    static class TestEntity {
        public Long getId() {
            return 1L;
        }
        public String getUserName() {
            return "user";
        }


    }
    static class TestView {
    }

    static class TestTableResolver implements TableResolver {
        @Override
        public <T> TableMeta resolve(Class<T> entityClass) {
            if (!TestEntity.class.equals(entityClass)) {
                return null;
            }
            return new TableMeta("test_entity", "id", Collections.singletonMap("id", "a.id"));
        }
    }

    static class TestBoosterMapper implements BoosterMapper<TestEntity, TestView> {
        @Override
        public List<TestView> selectByXml(SqlContext<TestEntity> sqlContext, Object page) {
            return Collections.emptyList();
        }
    }
}