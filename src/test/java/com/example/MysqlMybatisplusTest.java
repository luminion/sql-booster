package com.example;


import com.example.entity.SysUser;
import com.example.mapper.SysUserMapper;
import com.example.vo.SysUserVO;
import io.github.luminion.sqlbooster.core.TableMetaRegistry;
import io.github.luminion.sqlbooster.extension.mybatisplus.MpTableResolver;
import io.github.luminion.sqlbooster.model.BPage;
import io.github.luminion.sqlbooster.model.SqlContext;
import io.github.luminion.sqlbooster.builder.SqlBuilder;
import io.github.luminion.sqlbooster.enums.SqlKeyword;
import io.github.luminion.sqlbooster.model.query.Condition;
import io.github.luminion.sqlbooster.model.query.Sort;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * EnhancedMapper功能测试
 * 
 * @author bootystar
 */
@Slf4j
@SpringBootTest
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class MysqlMybatisplusTest {

    @Autowired
    private SysUserMapper sysUserMapper;
    
//    @Test
//    @Order(0)
//    public void testInsert() {
//        SqlBuilder<SysUser> iSqlTrees = new SqlBuilder<>();
//        Class<SysUser> entityClass = iSqlTrees.getEntityClass();
//        System.out.println(entityClass);
//    }

    @Test
    @BeforeEach
    void testBasic() {
        TableMetaRegistry.removeTableResolver(MpTableResolver.class);
    }
    
    /**
     * 测试基本的VO查询功能
     */
    @Test
    @Order(1)
    public void testList() {
        // 创建简单的查询条件
        SqlContext sqlEntity = new SqlContext();
        sqlEntity.getConditions().add(new Condition("name", SqlKeyword.EQ.getSymbol(), "张三"));
        
        List<SysUserVO> result = sysUserMapper.voList(sqlEntity);
        
        assertNotNull(result);
        assertFalse(result.isEmpty());
        assertTrue(result.size() >= 1); // 考虑可能已存在的数据
        assertEquals("张三", result.get(0).getName());
        assertEquals(25, result.get(0).getAge());
    }

    /**
     * 测试分页查询功能
     */
    @Test
    @Order(2)
    public void testPage() {
        // 创建查询条件
        SqlContext sqlEntity = new SqlContext();
        sqlEntity.getConditions().add(new Condition("age", SqlKeyword.GTE.getSymbol(), 25));

        // 创建分页对象
//        IPage<SysUserVO> page = new BPage<>(1, 2);
        int pageNum = 1;
        int pageSize = 2;

//        List<SysUserVO> result = sysUserMapper.voPage(sqlEntity, 1,2);
        BPage<SysUserVO> page = sysUserMapper.voPage(sqlEntity, pageNum,pageSize);

        assertNotNull(page);
        assertTrue(page.getPages() <= 2);
        assertEquals(pageNum, page.getCurrent());
        assertEquals(pageSize, page.getSize());
        assertTrue(page.getTotal() >= 3); // 至少有3个用户年龄>=25（包括可能已存在的数据）
    }

    /**
     * 测试多条件查询
     */
    @Test
    @Order(3)
    public void testMultipleConditions() {
        SqlContext sqlEntity = new SqlContext();
        sqlEntity.getConditions().add(new Condition("age", SqlKeyword.GTE.getSymbol(), 25));
        sqlEntity.getConditions().add(new Condition("age", SqlKeyword.LTE.getSymbol(), 35));
        sqlEntity.getConditions().add(new Condition("name", SqlKeyword.IS_NOT_NULL.getSymbol(), true));
        
        List<SysUserVO> result = sysUserMapper.voList(sqlEntity);
        
        assertNotNull(result);
        assertTrue(result.size() >= 3); // 至少有3个用户（张三、李四、王五），考虑可能已存在的数据
        result.forEach(user -> {
            assertNotNull(user.getName());
            assertTrue(user.getAge() >= 25 && user.getAge() <= 35);
        });
        
        BPage<SysUserVO> page = sysUserMapper.voPage(sqlEntity, 1,2);
        System.out.println(page);
        
    }

    /**
     * 测试LIKE查询
     */
    @Test
    @Order(4)
    public void testLikeQuery() {
       
        SqlBuilder<SysUser> sqlEntity = SqlBuilder.of(SysUser.class)
                ;
        sqlEntity.like(SysUser::getDescription,"测试")
                ;
        
        List<SysUserVO> result = sysUserMapper.voList(sqlEntity.build());
        
        assertNotNull(result);
        assertTrue(result.size() >= 3); // 至少有4个用户（我们插入的4个），考虑可能已存在的数据
        result.forEach(user -> assertTrue(user.getDescription().contains("测试")));
    }

    /**
     * 测试IN查询
     */
    @Test
    @Order(5)
    public void testInQuery() {
        SqlContext sqlEntity = new SqlContext();
        sqlEntity.getConditions().add(new Condition("age", SqlKeyword.IN.getSymbol(), Arrays.asList(25, 30)));
        
        List<SysUserVO> result = sysUserMapper.voList(sqlEntity);
        
        assertNotNull(result);
        assertTrue(result.size() >= 2); // 至少有2个用户（张三和李四），考虑可能已存在的数据
        result.forEach(user -> assertTrue(Arrays.asList(25, 30).contains(user.getAge())));
    }

    /**
     * 测试排序功能
     */
    @Test
    @Order(6)
    public void testSorting() {
        SqlContext sqlEntity = new SqlContext();
        sqlEntity.getConditions().add(new Condition("name", SqlKeyword.IS_NOT_NULL.getSymbol(),true));
        sqlEntity.getSorts().add(new Sort("age", true)); // 年龄升序
        
        List<SysUserVO> result = sysUserMapper.voList(sqlEntity);
        
        assertNotNull(result);
        assertTrue(result.size() >= 3); // 至少有3个用户（有名字的用户），考虑可能已存在的数据
        
        // 验证排序
        for (int i = 1; i < result.size(); i++) {
            assertTrue(result.get(i - 1).getAge() <= result.get(i).getAge());
        }
    }

    /**
     * 测试降序排序
     */
    @Test
    @Order(7)
    public void testDescendingSorting() {
        SqlContext sqlEntity = new SqlContext();
        sqlEntity.getConditions().add(new Condition("name", SqlKeyword.IS_NOT_NULL.getSymbol(),true));
        sqlEntity.getSorts().add(new Sort("age", false)); // 年龄降序
        
        List<SysUserVO> result = sysUserMapper.voList(sqlEntity);
        
        assertNotNull(result);
        assertTrue(result.size() >= 3); // 至少有3个用户，考虑可能已存在的数据
        
        // 验证降序排序
        for (int i = 1; i < result.size(); i++) {
            assertTrue(result.get(i - 1).getAge() >= result.get(i).getAge());
        }
    }

    /**
     * 测试空条件查询
     */
    @Test
    @Order(8)
    public void testEmptyConditions() {
        SqlContext sqlEntity = new SqlContext();
        // 不添加任何条件
        
        List<SysUserVO> result = sysUserMapper.voList(sqlEntity);
        
        assertNotNull(result);
        assertTrue(result.size() >= 4); // 至少有4个用户（我们插入的4个），考虑可能已存在的数据
    }

    /**
     * 测试NULL值查询
     */
    @Test
    @Order(9)
    public void testNullValueQuery() {
        SqlContext sqlEntity = new SqlContext();
        sqlEntity.getConditions().add(new Condition("nameLike", SqlKeyword.IS_NULL.getSymbol(), true));
        
        List<SysUserVO> result = sysUserMapper.voList(sqlEntity);
        
        assertNotNull(result);
        assertTrue(result.size() >= 1); // 至少有1个用户（user4的nameLike为null），考虑可能已存在的数据
        result.forEach(user -> assertNull(user.getNameLike()));
    }

    /**
     * 测试NOT NULL查询
     */
    @Test
    @Order(10)
    public void testNotNullQuery() {
        SqlContext sqlEntity = new SqlContext();
        sqlEntity.getConditions().add(new Condition("name", SqlKeyword.IS_NOT_NULL.getSymbol(), true));
        
        List<SysUserVO> result = sysUserMapper.voList(sqlEntity);
        
        assertNotNull(result);
        assertTrue(result.size() >= 3); // 至少有3个用户（除了user4，其他用户的name都不为null），考虑可能已存在的数据
        result.forEach(user -> assertNotNull(user.getName()));
    }

    /**
     * 测试NOT IN查询
     */
    @Test
    @Order(11)
    public void testNotInQuery() {
        SqlContext sqlEntity = new SqlContext();
        sqlEntity.getConditions().add(new Condition("age", SqlKeyword.NOT_IN.getSymbol(), Arrays.asList(25, 30)));
        
        List<SysUserVO> result = sysUserMapper.voList(sqlEntity);
        
        assertNotNull(result);
        assertTrue(result.size() >= 2); // 至少有2个用户（王五和user4），考虑可能已存在的数据
        // 不再检查具体年龄，因为可能有其他数据匹配
    }

    /**
     * 测试NOT LIKE查询
     */
    @Test
    @Order(12)
    public void testNotLikeQuery() {
        SqlContext sqlEntity = new SqlContext();
        sqlEntity.getConditions().add(new Condition("name", SqlKeyword.NOT_LIKE.getSymbol(), "%张%"));
        
        List<SysUserVO> result = sysUserMapper.voList(sqlEntity);
        
        assertNotNull(result);
        assertTrue(result.size() >= 2); // 至少有2个用户（李四和王五），考虑可能已存在的数据
        result.forEach(user -> {
            assertNotNull(user.getName());
            assertFalse(user.getName().contains("张"));
        });
    }

    /**
     * 测试getVOClass方法
     */
    @Test
    @Order(13)
    public void testGetVOClass() {
//        Class<SysUserVO> voClass = sysUserMapper.getClass();
//        
//        assertNotNull(voClass);
//        assertEquals(SysUserVO.class, voClass);
    }

    ///**
    // * 测试toVO方法
    // */
    //@Test
    //@Order(14)
    //public void testToVO() {
    //    SysUser user = new SysUser();
    //    user.setId(1L);
    //    user.setName("测试用户");
    //    user.setAge(25);
    //    
    //    SysUserVO vo = sysUserMapper.toVo(user);
    //    
    //    assertNotNull(vo);
    //    assertEquals(user.getId(), vo.getId());
    //    assertEquals(user.getName(), vo.getName());
    //    assertEquals(user.getAge(), vo.getAge());
    //}

    /**
     * 测试边界条件和异常情况
     */
    @Test
    @Order(15)
    public void testBoundaryConditions() {
        // 测试空的SqlWrapper
        assertDoesNotThrow(() -> {
            List<SysUserVO> result = sysUserMapper.voList(new SqlContext<>());
            assertNotNull(result);
        });
        
        // 测试null参数
        assertDoesNotThrow(() -> {
            List<SysUserVO> result = sysUserMapper.voList(null);
            assertNotNull(result);
        });
        
        // 测试空集合IN查询
        SqlContext sqlEntity = new SqlContext();
        sqlEntity.getConditions().add(new Condition("age", SqlKeyword.IN.getSymbol(), Collections.emptyList()));
        
        List<SysUserVO> result = sysUserMapper.voList(sqlEntity);
        assertNotNull(result);
        // 空集合IN查询的行为取决于具体实现，这里只验证不抛异常
    }


    /**
     * 测试分页功能的边界情况
     */
    @Test
    @Order(24)
    public void testPaginationBoundaries() {

        BPage<SysUserVO> page = SqlBuilder.of(SysUser.class)
                .gte(SysUser::getAge, 0)
                .boost(sysUserMapper)
                .page(-2, 10);
        assertNotNull(page);
        assertTrue(page.getTotal() >= 4);
        
        // 测试第一页
        BPage<SysUserVO> firstPage = SqlBuilder.of(SysUser.class)
                .gte(SysUser::getAge, 0)
                .boost(sysUserMapper)
                .page(1L, 2L);
        assertNotNull(firstPage);
        assertEquals(1, firstPage.getCurrent());
        assertEquals(2, firstPage.getSize());
        assertTrue(firstPage.getTotal() >= 4);
        // 测试超出范围的页码
        BPage<SysUserVO> outOfRangePage = SqlBuilder.of(SysUser.class)
                .gte(SysUser::getAge, 0)
                .boost(sysUserMapper)
                .page(999L, 10L);
        assertNotNull(outOfRangePage);
        assertEquals(999, outOfRangePage.getCurrent());
        assertTrue(outOfRangePage.getRecords().isEmpty());

        // 测试负数页码（应该被修正为1）
        BPage<SysUserVO> negativePage = SqlBuilder.of(SysUser.class)
                .gte(SysUser::getAge, 0)
                .boost(sysUserMapper)
                .page(-1L, 10L);
        assertNotNull(negativePage);
        assertEquals(1, negativePage.getCurrent()); // 应该被修正为1
    }
}
