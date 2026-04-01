package io.github.luminion.sqlbooster.builder;

import io.github.luminion.sqlbooster.core.Booster;
import io.github.luminion.sqlbooster.metadata.TableMeta;
import io.github.luminion.sqlbooster.metadata.TableMetaRegistry;
import io.github.luminion.sqlbooster.metadata.TableResolver;
import io.github.luminion.sqlbooster.model.BPage;
import io.github.luminion.sqlbooster.model.Condition;
import io.github.luminion.sqlbooster.model.ConditionSegment;
import io.github.luminion.sqlbooster.model.Sort;
import io.github.luminion.sqlbooster.model.SqlContext;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class SqlBuilderTest {

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
    public void shouldBuildComparisonConditions() {
        SqlContext<TestEntity> context = SqlBuilder.of(TestEntity.class)
                .eq(TestEntity::getName, "张三")
                .ne(TestEntity::getName, "李四")
                .gt(TestEntity::getAge, 18)
                .gte(TestEntity::getAge, 20)
                .lt(TestEntity::getAge, 60)
                .lte(TestEntity::getAge, 50)
                .toSqlContext();

        Iterator<Condition> iterator = context.getConditions().iterator();
        assertCondition(iterator.next(), "a.name", "=", "张三");
        assertCondition(iterator.next(), "a.name", "<>", "李四");
        assertCondition(iterator.next(), "a.age", ">", 18);
        assertCondition(iterator.next(), "a.age", ">=", 20);
        assertCondition(iterator.next(), "a.age", "<", 60);
        assertCondition(iterator.next(), "a.age", "<=", 50);
        Assert.assertFalse(iterator.hasNext());
    }

    @Test
    public void shouldBuildLikeInNullAndBitConditions() {
        SqlContext<TestEntity> context = SqlBuilder.of(TestEntity.class)
                .like(TestEntity::getDescription, "测试")
                .notLike(TestEntity::getName, "%张%")
                .in(TestEntity::getAge, Arrays.asList(25, 30))
                .notIn(TestEntity::getAge, Arrays.asList(35, 40))
                .isNull(TestEntity::getNameLike)
                .isNotNull(TestEntity::getCreateTime)
                .bitAny(TestEntity::getState, 1)
                .bitAll(TestEntity::getState, 3)
                .bitNone(TestEntity::getState, 2)
                .toSqlContext();

        Iterator<Condition> iterator = context.getConditions().iterator();
        assertCondition(iterator.next(), "a.description", "LIKE", "%测试%");
        assertCondition(iterator.next(), "a.name", "NOT LIKE", "%张%");
        assertCondition(iterator.next(), "a.age", "IN", Arrays.asList(25, 30));
        assertCondition(iterator.next(), "a.age", "NOT IN", Arrays.asList(35, 40));
        assertCondition(iterator.next(), "a.name_like", "IS NULL", true);
        assertCondition(iterator.next(), "a.create_time", "IS NOT NULL", true);
        assertCondition(iterator.next(), "a.state", "HAS ANY BITS", 1);
        assertCondition(iterator.next(), "a.state", "HAS ALL BITS", 3);
        assertCondition(iterator.next(), "a.state", "HAS NO BITS", 2);
        Assert.assertFalse(iterator.hasNext());
    }

    @Test
    public void shouldPreserveOrSegmentsAndSorts() {
        SqlContext<TestEntity> context = SqlBuilder.of(TestEntity.class)
                .gte(TestEntity::getState, 1)
                .or(segment -> segment.eq(TestEntity::getName, "李四").eq(TestEntity::getName, "张三"))
                .or(segment -> segment.gte(TestEntity::getAge, 20).lte(TestEntity::getAge, 30))
                .orderByDesc(TestEntity::getAge)
                .orderByAsc(TestEntity::getState)
                .toSqlContext();

        Iterator<ConditionSegment> segments = context.iterator();
        ConditionSegment root = segments.next();
        ConditionSegment firstOr = segments.next();
        ConditionSegment secondOr = segments.next();

        Assert.assertTrue(root.isAnd());
        Assert.assertFalse(firstOr.isAnd());
        Assert.assertFalse(secondOr.isAnd());
        assertCondition(root.getConditions().iterator().next(), "a.state", ">=", 1);

        Iterator<Condition> firstOrConditions = firstOr.getConditions().iterator();
        assertCondition(firstOrConditions.next(), "a.name", "=", "李四");
        assertCondition(firstOrConditions.next(), "a.name", "=", "张三");

        Iterator<Condition> secondOrConditions = secondOr.getConditions().iterator();
        assertCondition(secondOrConditions.next(), "a.age", ">=", 20);
        assertCondition(secondOrConditions.next(), "a.age", "<=", 30);

        Iterator<Sort> sorts = context.getSorts().iterator();
        assertSort(sorts.next(), "a.age", false);
        assertSort(sorts.next(), "a.state", true);
        Assert.assertFalse(sorts.hasNext());
    }

    @Test
    public void shouldIgnoreNullAndEmptyInputs() {
        SqlContext<TestEntity> context = SqlBuilder.of(TestEntity.class)
                .eq(TestEntity::getName, null)
                .ne(TestEntity::getName, null)
                .like(TestEntity::getDescription, null)
                .gt(TestEntity::getAge, null)
                .in(TestEntity::getAge, Collections.<Integer>emptyList())
                .notIn(TestEntity::getAge, Collections.<Integer>emptyList())
                .hasAllBits(TestEntity::getState, null)
                .fromMap(Collections.singletonMap("name", null))
                .toSqlContext();

        Assert.assertTrue(context.getConditions().isEmpty());
        Assert.assertEquals(1, context.getParams().size());
        Assert.assertEquals(Collections.emptyList(), context.getParams().get("age"));
        Assert.assertTrue(context.getSorts().isEmpty());
    }

    @Test
    public void shouldApplyImportedSuffixScenariosFromMapAndBean() {
        Map<String, Object> queryParams = new LinkedHashMap<>();
        queryParams.put("nameLikeLike", "三");
        queryParams.put("ageGe", 25);
        queryParams.put("ageLe", 35);

        SqlContext<TestEntity> mapContext = SqlBuilder.of(TestEntity.class)
                .appendEqByMap(queryParams)
                .toSqlContext();

        assertContainsCondition(mapContext, "a.name_like", "LIKE", "%三%");
        assertContainsCondition(mapContext, "a.age", ">=", 25);
        assertContainsCondition(mapContext, "a.age", "<=", 35);
        Assert.assertEquals(3, mapContext.getConditions().size());

        SuffixBean suffixBean = new SuffixBean();
        suffixBean.setNameLikeLike("四");
        suffixBean.setAgeGe(20);

        SqlContext<TestEntity> beanContext = SqlBuilder.of(TestEntity.class)
                .fromBean(suffixBean)
                .toSqlContext();

        assertContainsCondition(beanContext, "a.name_like", "LIKE", "%四%");
        assertContainsCondition(beanContext, "a.age", ">=", 20);
        Assert.assertEquals(2, beanContext.getConditions().size());
    }

    @Test
    public void shouldMergeCustomSuffixesWithDefaultsWhenRequested() {
        Map<String, Object> queryParams = new LinkedHashMap<>();
        queryParams.put("descriptionLike", "张");
        queryParams.put("nicknamePrefix", "A");

        SqlContext<TestEntity> context = SqlBuilder.of(TestEntity.class)
                .fromMap(queryParams, Collections.singletonMap("Prefix", "LIKE"), true)
                .toSqlContext();

        Iterator<Condition> iterator = context.getConditions().iterator();
        assertCondition(iterator.next(), "a.description", "LIKE", "%张%");
        assertCondition(iterator.next(), "a.nickname", "LIKE", "%A%");
        Assert.assertFalse(iterator.hasNext());
    }

    @Test
    public void shouldDelegateBoostOperations() {
        RecordingBooster booster = new RecordingBooster();

        List<TestView> list = SqlBuilder.of(TestEntity.class)
                .eq(TestEntity::getName, "张三")
                .boost(booster)
                .list();
        Assert.assertEquals(1, list.size());
        Assert.assertEquals("list", booster.lastCall);
        assertCondition(booster.lastContext.getConditions().iterator().next(), "a.name", "=", "张三");

        TestView first = SqlBuilder.of(TestEntity.class)
                .eq(TestEntity::getName, "李四")
                .boost(booster)
                .first();
        Assert.assertNotNull(first);
        Assert.assertEquals("first", booster.lastCall);

        BPage<TestView> page = SqlBuilder.of(TestEntity.class)
                .gte(TestEntity::getAge, 18)
                .boost(booster)
                .page(2L, 5L);
        Assert.assertNotNull(page);
        Assert.assertEquals("page", booster.lastCall);
        Assert.assertEquals(2L, booster.lastPageNum);
        Assert.assertEquals(5L, booster.lastPageSize);
    }

    private static void assertCondition(Condition condition, String field, String operator, Object value) {
        Assert.assertEquals(field, condition.getField());
        Assert.assertEquals(operator, condition.getOperator());
        Assert.assertEquals(value, condition.getValue());
    }

    private static void assertContainsCondition(SqlContext<TestEntity> context, String field, String operator, Object value) {
        for (Condition condition : context.getConditions()) {
            if (field.equals(condition.getField())
                    && operator.equals(condition.getOperator())
                    && value.equals(condition.getValue())) {
                return;
            }
        }
        Assert.fail("Condition not found: " + field + " " + operator + " " + value);
    }

    private static void assertSort(Sort sort, String field, boolean asc) {
        Assert.assertEquals(field, sort.getField());
        Assert.assertEquals(asc, sort.isAsc());
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
            propertyToColumnMap.put("age", "a.age");
            propertyToColumnMap.put("state", "a.state");
            propertyToColumnMap.put("description", "a.description");
            propertyToColumnMap.put("nameLike", "a.name_like");
            propertyToColumnMap.put("createTime", "a.create_time");
            propertyToColumnMap.put("nickname", "a.nickname");
            return new TableMeta("test_entity", "id", propertyToColumnMap);
        }
    }

    private static final class RecordingBooster implements Booster<TestEntity, TestView> {
        private String lastCall;
        private SqlContext<TestEntity> lastContext;
        private long lastPageNum;
        private long lastPageSize;

        @Override
        public TestView voById(Serializable id) {
            return new TestView();
        }

        @Override
        public List<TestView> voByIds(java.util.Collection<? extends Serializable> ids) {
            return Collections.singletonList(new TestView());
        }

        @Override
        public TestView voFirst(SqlContext<TestEntity> sqlContext) {
            this.lastCall = "first";
            this.lastContext = sqlContext;
            return new TestView();
        }

        @Override
        public TestView voUnique(SqlContext<TestEntity> sqlContext) {
            this.lastCall = "unique";
            this.lastContext = sqlContext;
            return new TestView();
        }

        @Override
        public List<TestView> voList(SqlContext<TestEntity> sqlContext) {
            this.lastCall = "list";
            this.lastContext = sqlContext;
            return Collections.singletonList(new TestView());
        }

        @Override
        public BPage<TestView> voPage(SqlContext<TestEntity> sqlContext, long pageNum, long pageSize) {
            this.lastCall = "page";
            this.lastContext = sqlContext;
            this.lastPageNum = pageNum;
            this.lastPageSize = pageSize;
            return new SimplePage<TestView>(Collections.singletonList(new TestView()), 1L, pageNum, pageSize);
        }
    }

    private static final class SimplePage<T> implements BPage<T> {
        private static final long serialVersionUID = 1L;

        private final List<T> records;
        private final long total;
        private final long current;
        private final long size;

        private SimplePage(List<T> records, long total, long current, long size) {
            this.records = records;
            this.total = total;
            this.current = current;
            this.size = size;
        }

        @Override
        public List<T> getRecords() {
            return records;
        }

        @Override
        public long getTotal() {
            return total;
        }

        @Override
        public long getCurrent() {
            return current;
        }

        @Override
        public long getSize() {
            return size;
        }

        @Override
        public <R> BPage<R> convertRecords(Class<R> clazz) {
            return new SimplePage<R>(Collections.<R>emptyList(), total, current, size);
        }
    }

    private static final class TestEntity {
        private Long id;
        private String name;
        private Integer age;
        private Integer state;
        private String description;
        private String nameLike;
        private Date createTime;
        private String nickname;

        public Long getId() {
            return id;
        }

        public String getName() {
            return name;
        }

        public Integer getAge() {
            return age;
        }

        public Integer getState() {
            return state;
        }

        public String getDescription() {
            return description;
        }

        public String getNameLike() {
            return nameLike;
        }

        public Date getCreateTime() {
            return createTime;
        }

        public String getNickname() {
            return nickname;
        }
    }

    public static final class SuffixBean {
        private String nameLikeLike;
        private Integer ageGe;

        public String getNameLikeLike() {
            return nameLikeLike;
        }

        public void setNameLikeLike(String nameLikeLike) {
            this.nameLikeLike = nameLikeLike;
        }

        public Integer getAgeGe() {
            return ageGe;
        }

        public void setAgeGe(Integer ageGe) {
            this.ageGe = ageGe;
        }
    }

    private static final class TestView {
    }
}
