package io.github.luminion.sqlbooster.util;

import io.github.luminion.sqlbooster.builder.SqlBuilder;
import io.github.luminion.sqlbooster.metadata.TableMeta;
import io.github.luminion.sqlbooster.metadata.TableMetaRegistry;
import io.github.luminion.sqlbooster.metadata.TableResolver;
import io.github.luminion.sqlbooster.model.Condition;
import io.github.luminion.sqlbooster.model.ConditionSegment;
import io.github.luminion.sqlbooster.model.SqlContext;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;

public class SqlContextUtilsTest {

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
    public void buildWithSuffixShouldKeepSegmentStructure() {
        SqlContext<TestEntity> source = new SqlContext<>();
        source.getConditions().add(new Condition("id", "=", 1L));

        ConditionSegment orSegment = new ConditionSegment();
        orSegment.setAnd(false);
        orSegment.getConditions().add(new Condition("nameLike", "=", "tom"));
        source.setNext(orSegment);

        SqlContext<TestEntity> result = SqlContextUtils.buildWithSuffix(TestEntity.class, source);

        Iterator<ConditionSegment> iterator = result.iterator();
        ConditionSegment first = iterator.next();
        ConditionSegment second = iterator.next();

        Condition rootCondition = first.getConditions().iterator().next();
        Condition orCondition = second.getConditions().iterator().next();

        Assert.assertTrue(first.isAnd());
        Assert.assertFalse(second.isAnd());
        Assert.assertEquals("a.id", rootCondition.getField());
        Assert.assertEquals("=", rootCondition.getOperator());
        Assert.assertEquals(1L, rootCondition.getValue());
        Assert.assertEquals("a.name", orCondition.getField());
        Assert.assertEquals("LIKE", orCondition.getOperator());
        Assert.assertEquals("%tom%", orCondition.getValue());
        Assert.assertFalse(iterator.hasNext());
    }

    @Test
    public void buildWithSuffixShouldKeepLegacyCustomSuffixReplacementBehavior() {
        SqlContext<TestEntity> source = new SqlContext<>();
        source.getConditions().add(new Condition("nameLike", "=", "tom"));

        SqlContext<TestEntity> result = SqlContextUtils.buildWithSuffix(TestEntity.class, source,
                Collections.singletonMap("Prefix", "LIKE"));

        Assert.assertTrue(result.getConditions().isEmpty());
        Assert.assertEquals("tom", result.getParams().get("nameLike"));
    }

    @Test
    public void builderShouldSupportMergingCustomSuffixesWithDefaults() {
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("nameLike", "tom");
        params.put("nickPrefix", "jerry");

        SqlContext<TestEntity> result = SqlBuilder.of(TestEntity.class)
                .fromMap(params, Collections.singletonMap("Prefix", "LIKE"), true)
                .toSqlContext();

        Assert.assertTrue(result.getParams().isEmpty());
        Assert.assertEquals(2, result.getConditions().size());
    }

    private static final class TestTableResolver implements TableResolver {
        @Override
        public <T> TableMeta resolve(Class<T> entityClass) {
            if (entityClass != TestEntity.class) {
                return null;
            }
            Map<String, String> propertyToColumnMap = new LinkedHashMap<>();
            propertyToColumnMap.put("id", "a.id");
            propertyToColumnMap.put("name", "a.name");
            propertyToColumnMap.put("nick", "a.nick");
            return new TableMeta("test_entity", "id", propertyToColumnMap);
        }
    }

    private static final class TestEntity {
    }
}
