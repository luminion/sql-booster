package io.github.luminion.sqlbooster.metadata;

import io.github.luminion.sqlbooster.core.Booster;
import io.github.luminion.sqlbooster.model.BPage;
import io.github.luminion.sqlbooster.model.SqlContext;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;

public class BoosterRegistryTest {

    @Before
    @After
    public void clearRegistry() {
        BoosterRegistry.clear();
    }

    @Test
    public void shouldRegisterMultipleResultTypesForTheSameEntity() {
        StringBooster stringBooster = new StringBooster();
        IntegerBooster integerBooster = new IntegerBooster();

        BoosterRegistry.registerBooster("stringBooster", stringBooster);
        BoosterRegistry.registerBooster("integerBooster", integerBooster);

        Assert.assertSame(stringBooster, BoosterRegistry.getRequiredBooster(TestEntity.class, String.class));
        Assert.assertSame(integerBooster, BoosterRegistry.getRequiredBooster(TestEntity.class, Integer.class));
    }

    private abstract static class AbstractTestBooster<V> implements Booster<TestEntity, V> {
        @Override
        public V voById(Serializable id) {
            return null;
        }

        @Override
        public List<V> voByIds(java.util.Collection<? extends Serializable> ids) {
            return Collections.emptyList();
        }

        @Override
        public V voFirst(SqlContext<TestEntity> sqlContext) {
            return null;
        }

        @Override
        public V voUnique(SqlContext<TestEntity> sqlContext) {
            return null;
        }

        @Override
        public List<V> voList(SqlContext<TestEntity> sqlContext) {
            return Collections.emptyList();
        }

        @Override
        public BPage<V> voPage(SqlContext<TestEntity> sqlContext, long pageNum, long pageSize) {
            return null;
        }
    }

    private static final class StringBooster extends AbstractTestBooster<String> {
    }

    private static final class IntegerBooster extends AbstractTestBooster<Integer> {
    }

    private static final class TestEntity {
    }
}
