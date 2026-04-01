package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.metadata.TableMeta;
import io.github.luminion.sqlbooster.metadata.TableMetaRegistry;
import io.github.luminion.sqlbooster.metadata.TableResolver;
import io.github.luminion.sqlbooster.model.BPage;
import io.github.luminion.sqlbooster.model.Condition;
import io.github.luminion.sqlbooster.model.SqlContext;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class BoosterOperationsTest {

    private final TestTableResolver tableResolver = new TestTableResolver();

    @Before
    public void setUp() {
        TableMetaRegistry.addTableResolver(tableResolver);
    }

    @After
    public void tearDown() {
        TableMetaRegistry.removeTableResolver(tableResolver);
    }

    @Test
    public void voByIdsShouldReturnEmptyListWhenIdsAreNullOrEmpty() {
        RecordingBooster booster = new RecordingBooster();

        Assert.assertTrue(booster.voByIds(null).isEmpty());
        Assert.assertTrue(booster.voByIds(Collections.<Serializable>emptyList()).isEmpty());
        Assert.assertNull(booster.lastSqlContext);
    }

    @Test
    public void voByIdsShouldBuildInConditionWhenIdsPresent() {
        RecordingBooster booster = new RecordingBooster();

        booster.voByIds(Arrays.<Serializable>asList(1L, 2L));

        Assert.assertNotNull(booster.lastSqlContext);
        Condition condition = booster.lastSqlContext.getConditions().iterator().next();
        Assert.assertEquals("id", condition.getField());
        Assert.assertEquals("IN", condition.getOperator());
        Assert.assertEquals(Arrays.asList(1L, 2L), condition.getValue());
    }

    private static final class RecordingBooster implements BoosterOperations<TestEntity, String> {
        private SqlContext<TestEntity> lastSqlContext;

        @Override
        public String voById(Serializable id) {
            return null;
        }

        @Override
        public List<String> voList(SqlContext<TestEntity> sqlContext) {
            this.lastSqlContext = sqlContext;
            return Collections.singletonList("ok");
        }

        @Override
        public BPage<String> voPage(SqlContext<TestEntity> sqlContext, long pageNum, long pageSize) {
            return null;
        }
    }

    private static final class TestTableResolver implements TableResolver {
        @Override
        public <T> TableMeta resolve(Class<T> entityClass) {
            if (entityClass != TestEntity.class) {
                return null;
            }
            Map<String, String> propertyToColumnMap = new LinkedHashMap<>();
            propertyToColumnMap.put("id", "a.id");
            return new TableMeta("test_entity", "id", propertyToColumnMap);
        }
    }

    private static final class TestEntity {
    }
}
