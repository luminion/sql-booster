package io.github.luminion.sqlbooster.builder;

import io.github.luminion.sqlbooster.model.SqlContext;
import io.github.luminion.sqlbooster.model.Condition;
import io.github.luminion.sqlbooster.support.BoosterTestFixtures;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class SqlBuilderNormalizationTest {

    @Before
    public void setUp() {
        BoosterTestFixtures.registerTestResolver();
    }

    @After
    public void tearDown() {
        BoosterTestFixtures.clearRegistries();
    }

    @Test
    public void buildShouldNormalizeSuffixesAndKeepUnknownParams() {
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("nameLike", "tom");
        params.put("ageNe", 18);
        params.put("unknownFlag", true);

        SqlContext<BoosterTestFixtures.TestEntity> context = SqlBuilder.of(BoosterTestFixtures.TestEntity.class)
                .fromMap(params)
                .build();

        assertTrue(hasCondition(context, "name", "LIKE", "%tom%"));
        assertTrue(hasCondition(context, "age", "<>", 18));
        assertEquals(Boolean.TRUE, context.getParams().get("unknownFlag"));
    }

    @Test
    public void fromBeanShouldNormalizeBeanProperties() {
        BoosterTestFixtures.TestDto dto = new BoosterTestFixtures.TestDto();
        dto.setName("tom");
        dto.setState(1);

        SqlContext<BoosterTestFixtures.TestEntity> context = SqlBuilder.of(BoosterTestFixtures.TestEntity.class)
                .fromBean(dto)
                .build();

        assertTrue(hasCondition(context, "name", "=", "tom"));
        assertTrue(hasCondition(context, "state", "=", 1));
    }

    @Test
    @SuppressWarnings("deprecation")
    public void appendByMapShouldDelegateToFromMap() {
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("nameLike", "tom");

        SqlContext<BoosterTestFixtures.TestEntity> context = SqlBuilder.of(BoosterTestFixtures.TestEntity.class)
                .appendByMap(params)
                .build();

        assertTrue(hasCondition(context, "name", "LIKE", "%tom%"));
    }

    @Test
    public void fromMapShouldFallbackInvalidCollectionValueToParams() {
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("stateIn", Collections.<Integer>emptyList());

        SqlContext<BoosterTestFixtures.TestEntity> context = SqlBuilder.of(BoosterTestFixtures.TestEntity.class)
                .fromMap(params)
                .build();

        assertFalse(hasCondition(context, "state", "IN", Collections.emptyList()));
        assertEquals(Collections.emptyList(), context.getParams().get("stateIn"));
    }

    @Test
    public void lambdaShouldFallbackInvalidCollectionValueToParams() {
        SqlContext<BoosterTestFixtures.TestEntity> context = SqlBuilder.of(BoosterTestFixtures.TestEntity.class)
                .in(BoosterTestFixtures.TestEntity::getState, Collections.<Integer>emptyList())
                .build();

        assertTrue(context.getConditions().isEmpty());
        assertEquals(Collections.emptyList(), context.getParams().get("state"));
    }

    @Test
    public void appendShouldFallbackInvalidOperatorToParams() {
        SqlContext<BoosterTestFixtures.TestEntity> context = SqlBuilder.of(BoosterTestFixtures.TestEntity.class)
                .append(new Condition("state", "invalid", 1))
                .build();

        assertTrue(context.getConditions().isEmpty());
        assertEquals(1, context.getParams().get("state"));
    }

    @Test
    public void appendSqlContextShouldNormalizeDefaultSuffixes() {
        SqlContext<BoosterTestFixtures.TestEntity> source = new SqlContext<>();
        source.getConditions().add(new Condition("nameLike", "=", "tom"));

        SqlContext<BoosterTestFixtures.TestEntity> context = SqlBuilder.of(BoosterTestFixtures.TestEntity.class)
                .append(source)
                .build();

        assertTrue(hasCondition(context, "name", "LIKE", "%tom%"));
    }

    @Test
    public void appendSqlContextShouldSupportCustomSuffixOverload() {
        SqlContext<BoosterTestFixtures.TestEntity> source = new SqlContext<>();
        source.getConditions().add(new Condition("age_ge", "=", 18));

        SqlContext<BoosterTestFixtures.TestEntity> context = SqlBuilder.of(BoosterTestFixtures.TestEntity.class)
                .append(source, Collections.singletonMap("_ge", ">="))
                .build();

        assertTrue(hasCondition(context, "age", ">=", 18));
    }

    @Test
    public void fromMapShouldSupportCustomSuffixOverload() {
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("age_ge", 18);

        SqlContext<BoosterTestFixtures.TestEntity> context = SqlBuilder.of(BoosterTestFixtures.TestEntity.class)
                .fromMap(params, Collections.singletonMap("_ge", ">="))
                .build();

        assertTrue(hasCondition(context, "age", ">=", 18));
    }

    @Test
    public void fromBeanShouldSupportCustomSuffixOverload() {
        CustomSuffixDto dto = new CustomSuffixDto();
        dto.setAge_ge(18);

        SqlContext<BoosterTestFixtures.TestEntity> context = SqlBuilder.of(BoosterTestFixtures.TestEntity.class)
                .fromBean(dto, Collections.singletonMap("_ge", ">="))
                .build();

        assertTrue(hasCondition(context, "age", ">=", 18));
    }

    public static class CustomSuffixDto {
        private Integer age_ge;

        public Integer getAge_ge() {
            return age_ge;
        }

        public void setAge_ge(Integer age_ge) {
            this.age_ge = age_ge;
        }
    }

    private boolean hasCondition(SqlContext<?> context, String field, String operator, Object value) {
        for (Condition condition : context.getConditions()) {
            if (field.equals(condition.getField())
                    && operator.equals(condition.getOperator())
                    && value.equals(condition.getValue())) {
                return true;
            }
        }
        return false;
    }
}
