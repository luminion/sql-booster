# SQL Booster

[![Maven Central](https://img.shields.io/maven-central/v/io.github.luminion/sql-booster)](https://mvnrepository.com/artifact/io.github.luminion/sql-booster)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)
[![GitHub stars](https://img.shields.io/github/stars/luminion/sql-booster?style=social)](https://github.com/luminion/sql-booster)

SQL Booster 是一个数据访问层（DAO）增强工具包。主要提供动态后缀映射、多层级条件拼装等结构化查询构建功能。

> 历史版本: [mybatis-plus-enhancer](https://github.com/bootystar/mybatis-plus-enhancer)

## 功能特性

- **后缀映射查询**：支持通过 `字段名` + `特定后缀` 自动映射为不同类型的 SQL 查询条件。
- **动态条件拼装**：支持根据入参（JSON / 对象）动态构建多层级、嵌套的 SQL 条件。
- **参数类型转换**：自动转化 Map 参数，自动将 Java 属性映射为数据库字段。
- **查询结果映射**：自动将数据库查询结果转换为指定的 VO / DTO 类。
- **防 SQL 注入**：参数解析阶段包含实体类字段合法性校验，底层通过预编译处理参数。
- **链式条件追加**：支持 Lambda 流式 API 组合查询条件。
- **基础查询扩展**：内置 `voById`、`voList`、`voPage` 等常用单表及分页查询方法。

---

## Maven 依赖

[![Maven Central](https://img.shields.io/maven-central/v/io.github.luminion/sql-booster)](https://mvnrepository.com/artifact/io.github.luminion/sql-booster)

```xml
<dependency>
    <groupId>io.github.luminion</groupId>
    <artifactId>sql-booster</artifactId>
    <version>latest</version>
</dependency>
```

<details>
<summary><b>获取 Snapshot 快照版本</b></summary>

添加 Maven 中央快照仓库：

```xml
<repositories>
    <repository>
        <id>central-portal-snapshots</id>
        <name>Central Portal Snapshots</name>
        <url>https://central.sonatype.com/repository/maven-snapshots/</url>
        <releases><enabled>false</enabled></releases>
        <snapshots><enabled>true</enabled></snapshots>
    </repository>
</repositories>

<dependencies>
    <dependency>
        <groupId>io.github.luminion</groupId>
        <artifactId>sql-booster</artifactId>
        <version>1.0.0-SNAPSHOT</version>
    </dependency>
</dependencies>
```
</details>

---

## 快速开始

配套的 [code-generator](https://github.com/luminion/code-generator) 代码生成器已适配本框架，支持生成相关结构代码。

### 1. Mapper继承接口

```java
import io.github.luminion.sqlbooster.extension.mybatisplus.MpMapper;

public interface SysUserMapper extends MpMapper<SysUser, SysUserVO> {
}
```

### 2. XML引入`sqlbooster`片段


- `sqlbooster.conditions` 会自动根据查询参数生成查询条件。
- `sqlbooster.sorts` 会自动根据查询参数生成排序条件。
```xml
<mapper namespace="com.example.mapper.SysUserMapper">

    <select id="selectByXml" resultType="com.example.vo.SysUserVO">
        SELECT a.* FROM sys_user a
        <where>
            <include refid="sqlbooster.conditions"/>
        </where>
        <trim prefix="ORDER BY" prefixOverrides=",">
            <include refid="sqlbooster.sorts"/>
            , a.id DESC
        </trim>
    </select>

</mapper>
```
可通过[BoosterMapperUtils](src/main/java/io/github/luminion/sqlbooster/util/BoosterMapperUtils.java) 传入mapper类一键获取完整Xml内容

---

### 可用扩展接口

提供以下接口, 按需使用/扩展

| 接口 / 类名 | 适用范围 | 所在包路径 |
|---|---|---|
| `BoosterMapper` | MyBatis Mapper 增强 (不含分页) | `io.github.luminion.sqlbooster.extension.mybatis` |
| `PhMapper` | 基于 PageHelper 的 Mapper 增强 | `io.github.luminion.sqlbooster.extension.pagehelper` |
| `MpMapper` | 基于 MyBatis-Plus `BaseMapper` 增强 | `io.github.luminion.sqlbooster.extension.mybatisplus` |
| `MpService` | 基于 MyBatis-Plus `IService` 增强 | `io.github.luminion.sqlbooster.extension.mybatisplus` |
| `MpServiceImpl` | 基于 MyBatis-Plus `ServiceImpl` 增强 | `io.github.luminion.sqlbooster.extension.mybatisplus` |
| `BoosterSupport` | 基础实现类，提供核心逻辑默认实现 | `io.github.luminion.sqlbooster.core` |


---

## 使用示例

查询支持 DTO、Map、Lambda 链式及 `SqlContext` 结构传参：

```java
import io.github.luminion.sqlbooster.builder.SqlBuilder;
import io.github.luminion.sqlbooster.extension.mybatis.BoosterMapper;
import io.github.luminion.sqlbooster.model.SqlContext;
import org.springframework.web.bind.annotation.*;
import org.springframework.beans.factory.annotation.Autowired;
import java.util.*;

@RestController
@RequestMapping("/user")
public class SysUserController {

    @Autowired
    private BoosterMapper<SysUser, SysUserVO> sysUserMapper;

    // 根据 ID 查询 VO
    @GetMapping("/{id}")
    public SysUserVO getUserById(@PathVariable Long id) {
        return sysUserMapper.voById(id);
    }

    // 通过 DTO 属性映射条件
    @PostMapping("/dto")
    public List<SysUserVO> getUsersByDTO(@RequestBody SysUserDTO dto) {
        SqlContext<SysUser> sqlContext = SqlBuilder.of(sysUserMapper)
                .appendByBean(dto)
                .build();
        return sysUserMapper.voList(sqlContext);
    }

    // 通过 Map 键值后缀映射条件
    @PostMapping("/map")
    public List<SysUserVO> getUsersByMap(@RequestBody Map<String, Object> params) {
        return sysUserMapper.lambdaBooster()
                .appendByMap(params)
                .list();
    }

    // 混合使用 Lambda 链式与 Map
    @PostMapping("/lambda")
    public List<SysUserVO> getByLambda(@RequestBody Map<String, Object> params) {
        return sysUserMapper.lambdaBooster()
                .appendByMap(params)               
                .eq(SysUser::getState, 1)          
                .gte(SysUser::getAge, 18)          
                .in(SysUser::getRoleId, Arrays.asList(1, 2)) 
                .like(SysUser::getUserName, "tom") 
                .list();
    }

    // 接收前端构建的 SqlContext (框架内含字段校验逻辑)
    @PostMapping("/sql")
    public List<SysUserVO> getUsersBySql(@RequestBody SqlContext<SysUser> sqlContext) {
        return sysUserMapper.voList(sqlContext);
    }

    // 分页查询
    @PostMapping("/page/{current}/{size}")
    public BPage<SysUserVO> getUserPage(@RequestBody Map<String, Object> params,
                                        @PathVariable("current") Long current,
                                        @PathVariable("size") Long size) {
        return sysUserMapper.lambdaBooster()
                .appendByMap(params)
                .page(current, size);
    }
}
```

---

## 核心功能详解

### 1. 动态后缀映射

当参数名称携带指定后缀时，将自动映射为对应的 SQL 查询类型。无后缀默认映射为 `=` 查询。

#### 后缀规则表

| 操作说明 | 后缀 | 示例（JSON Key） | 值类型 |
|---|---|---|---|
| 等于 | (无) | `"name": "mike"` | String, Number, Boolean |
| 不等于 | `Ne` | `"ageNe": 18` | String, Number, Boolean |
| 小于 | `Lt` | `"ageLt": 18` | Number, Date |
| 小于等于 | `Lte` | `"ageLte": 18` | Number, Date |
| 大于 | `Gt` | `"ageGt": 18` | Number, Date |
| 大于等于 | `Gte` | `"ageGte": 18` | Number, Date |
| 模糊匹配 | `Like` | `"nameLike": "mike"` | String |
| 反模糊匹配 | `NotLike` | `"nameNotLike": "mike"` | String |
| IN 查询 | `In` | `"stateIn": [1, 2, 3]` | List/Array (String, Number) |
| NOT IN 查询 | `NotIn` | `"stateNotIn": [1, 2, 3]` | List/Array (String, Number) |
| 为空 | `IsNull` | `"nameIsNull": true` | Boolean (true) |
| 不为空 | `IsNotNull` | `"nameIsNotNull": true` | Boolean (true) |
| 包含任意指定bit位 | `HasAnyBits` | `"roleHasAnyBits": 4` | Number |
| 包含所有指定bit位 | `HasAllBits` | `"roleHasAllBits": 4` | Number |
| 不包含指定bit位 | `HasNoBits` | `"roleHasNoBits": 4` | Number |

#### 入参示例

查询 `name` 包含 `mike`，`version` 为 `1`，`age` 在 `18-60` 之间，`state` 为 `1` 或 `2` 或 `3` 的数据：

```json
{
  "nameLike": "mike",
  "version": 1,
  "ageGte": 18,
  "ageLt": 60,
  "stateIn": [1, 2, 3]
}
```

#### 自定义映射规则

支持调整或替换默认的后缀映射规则：

```java
import io.github.luminion.sqlbooster.builder.SqlBuilder;
import io.github.luminion.sqlbooster.util.SqlContextUtils;

// 1. 全局替换默认后缀映射
HashMap<String, String> customMap = new HashMap<>();
customMap.put("_like", "LIKE");
customMap.put("_ge", ">=");
SqlContextUtils.refreshDefaultSuffixes(customMap);

// 2. 局部指定后缀映射
SqlContext<SysUser> sqlContext = SqlBuilder.of(SysUser.class)
        .build(entityClass, ctx -> SqlContextUtils.buildWithSuffix(entityClass, ctx, customMap));
```

---

### 2. BoosterInterceptor 拦截器

`BoosterInterceptor` 暴露了 `preHandle` 和 `postHandle` 方法，允许在查询执行前后对上下文及结果集进行干预。适用于逻辑删除、多租户隔离、数据脱敏等横切逻辑。

#### 接口定义与使用

```java
@Service
public class SysUserServiceImpl extends MpServiceImpl<SysUserMapper, SysUser, SysUserVO> {

    @Override
    public void preHandle(SqlContext<SysUser> context) {
        // 追加全局逻辑删除条件
        context.getConditions().add(new Condition("deleted", "=", 0));

        // 依据上下文注入数据隔离条件
        if (!isAdmin()) {
            context.getConditions().add(new Condition("createBy", "=", UserUtils.getUserId()));
        }
    }

    @Override
    public void postHandle(SqlContext<SysUser> context, List<SysUserVO> resultList) {
        // 对查询结果集进行后置处理（如字段掩码）
        resultList.forEach(vo -> {
            if (vo.getPhone() != null) {
                vo.setPhone(vo.getPhone().replaceAll("(\\d{3})\\d{4}(\\d{4})", "$1****$2"));
            }
        });
    }
}
```

---

### 3. SqlContext 结构解析

`SqlContext` 为底层统一数据结构，支持前端以 JSON 格式提交多层级组合条件。解析时依据实体类进行字段合法性匹配。

#### 操作符对照表 (operator)

| 说明 | operator 参数值 | 值类型 |
|:---|:---|:---|
| 等于 | 无需传参、`=`、`==`、`eq` | Any |
| 不等于 | `<>`、`!=`、`ne` | Any |
| 小于 | `<`、`lt` | Number, Date |
| 小于等于 | `<=`、`le`、`lte` | Number, Date |
| 大于 | `>`、`gt` | Number, Date |
| 大于等于 | `>=`、`ge`、`gte` | Number, Date |
| 模糊匹配 | `like` | String |
| 反模糊匹配 | `not like` | String |
| 在指定列表中 | `in` | List/Array (String, Number) |
| 不在指定列表中 | `not in` | List/Array (String, Number) |
| 为空 | `is null` | Boolean (true) |
| 不为空 | `is not null` | Boolean (true) |
| 包含任意指定bit位 | `has any bits` | Number |
| 包含所有指定bit位 | `has all bits` | Number |
| 不包含指定bit位 | `has no bits` | Number |

#### 基础条件与排序

查询 `name = 'mike'` 且 `version >= 1`，按 `id` 降序、`age` 升序排列：

```json
{
  "conditions": [
    { "field": "name", "value": "mike" },
    { "field": "version", "operator": ">=", "value": 1 }
  ],
  "sorts": [
    { "field": "id" },
    { "field": "age", "asc": true }
  ]
}
```

#### 条件层级嵌套

`SqlContext` 包含以下核心节点控制逻辑：
* `conditions`: 当前层级的条件节点数组。
* `and`: `true` 表示 `AND` 关系，`false` 表示 `OR` 关系（控制当前层级内节点间的联系）。
* `next`: 子级嵌套结构。
* `sorts`: 排序规则（仅限首层节点传递）。

**逻辑示例**：
目标 SQL 结构为：
```sql
WHERE (country = 'china' AND mobile IS NOT NULL)
  AND (name = 'mike' OR name = 'john')
  AND (age < 18 OR age > 60)
```

对应的 JSON 结构定义：
```json
{
  "conditions": [
    { "field": "country", "value": "china" },
    { "field": "mobile", "operator": "is_not_null", "value": true }
  ],
  "next": {
    "and": false,
    "conditions": [
      { "field": "name", "value": "mike" },
      { "field": "name", "value": "john" }
    ],
    "next": {
      "and": false,
      "conditions": [
        { "field": "age", "operator": "<", "value": 18 },
        { "field": "age", "operator": ">", "value": 60 }
      ]
    }
  }
}
```