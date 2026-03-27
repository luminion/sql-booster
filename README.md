# SQL Booster

[![Maven Central](https://img.shields.io/maven-central/v/io.github.luminion/sql-booster)](https://mvnrepository.com/artifact/io.github.luminion/sql-booster)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)
[![GitHub stars](https://img.shields.io/github/stars/luminion/sql-booster?style=social)](https://github.com/luminion/sql-booster)

SQL Booster 是一个数据访问层（DAO）增强工具包，主要提供动态后缀映射、多层级条件拼装、Lambda 条件构造和目标类型转换能力。

> 历史版本: [mybatis-plus-enhancer](https://github.com/bootystar/mybatis-plus-enhancer)

## 功能特性

- **后缀映射查询**：支持 `字段名 + 后缀` 自动映射成不同 SQL 条件。
- **Builder 默认安全**：`SqlBuilder.build()` / `toSqlContext()` 默认返回可直接交给 mapper 的安全 `SqlContext`。
- **Lambda 严格校验**：Lambda 链式条件在追加阶段立即校验，非法字段或非法值直接抛错。
- **Map / Bean 宽松模式**：未知字段可继续落入 `params`，方便 XML 自定义条件扩展。
- **目标类型转换**：默认 mapper 可继续查询 `VO`，也可显式转成任意 `DTO / VO` 类型。
- **实体直连 Booster**：实体可实现 `BoosterModel<T, V>`，直接使用 `entity.lambdaBooster()`。
- **自定义 Mapper 友好**：可通过 `toSqlContext()` 把构造器结果直接作为 mapper 入参复用 `conditions` / `sorts` 片段。
- **基础查询扩展**：内置 `voById`、`voList`、`voPage` 等常用单表查询方法。

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

### 1. Mapper 继承接口

```java
import io.github.luminion.sqlbooster.extension.mybatisplus.MpMapper;

public interface SysUserMapper extends MpMapper<SysUser, SysUserVO> {
}
```

### 2. XML 引入 `sqlbooster` 片段

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

可通过 [BoosterMapperUtils](src/main/java/io/github/luminion/sqlbooster/util/BoosterMapperUtils.java) 传入 mapper 类一键获取完整 XML 内容。

### 3. 可选：实体实现默认 Booster 接口

如果你希望实体直接使用 Booster，可让实体实现 `BoosterModel<T, V>`：

```java
import io.github.luminion.sqlbooster.core.BoosterModel;

public class SysUser implements BoosterModel<SysUser, SysUserVO> {
}
```

这样可以直接：

```java
SysUser user = new SysUser();
List<SysUserVO> list = user.lambdaBooster()
        .eq(SysUser::getState, 1)
        .like(SysUser::getUserName, "tom")
        .list();
```

---

## 可用扩展接口

| 接口 / 类名 | 适用范围 | 所在包路径 |
|---|---|---|
| `BoosterMapper` | MyBatis Mapper 增强 (不含分页) | `io.github.luminion.sqlbooster.extension.mybatis` |
| `PhMapper` | 基于 PageHelper 的 Mapper 增强 | `io.github.luminion.sqlbooster.extension.pagehelper` |
| `MpMapper` | 基于 MyBatis-Plus `BaseMapper` 增强 | `io.github.luminion.sqlbooster.extension.mybatisplus` |
| `MpService` | 基于 MyBatis-Plus `IService` 增强 | `io.github.luminion.sqlbooster.extension.mybatisplus` |
| `MpServiceImpl` | 基于 MyBatis-Plus `ServiceImpl` 增强 | `io.github.luminion.sqlbooster.extension.mybatisplus` |
| `BoosterModel` | 实体默认 Booster 入口 | `io.github.luminion.sqlbooster.core` |
| `EntityBoosters` | 实体 / 目标类型 Booster 工具类 | `io.github.luminion.sqlbooster.core` |
| `BoosterSupport` | 基础实现类，提供核心逻辑默认实现 | `io.github.luminion.sqlbooster.core` |

---

## 使用示例

### 1. Mapper / Builder / Lambda

```java
import io.github.luminion.sqlbooster.builder.SqlBuilder;
import io.github.luminion.sqlbooster.extension.mybatis.BoosterMapper;
import io.github.luminion.sqlbooster.model.BPage;
import io.github.luminion.sqlbooster.model.SqlContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/user")
public class SysUserController {

    @Autowired
    private BoosterMapper<SysUser, SysUserVO> sysUserMapper;

    @GetMapping("/{id}")
    public SysUserVO getUserById(@PathVariable Long id) {
        return sysUserMapper.voById(id);
    }

    @PostMapping("/dto")
    public List<SysUserVO> getUsersByDTO(@RequestBody SysUserDTO dto) {
        SqlContext<SysUser> sqlContext = SqlBuilder.of(sysUserMapper)
                .fromBean(dto)
                .build();
        return sysUserMapper.voList(sqlContext);
    }

    @PostMapping("/map")
    public List<SysUserVO> getUsersByMap(@RequestBody Map<String, Object> params) {
        return sysUserMapper.lambdaBooster()
                .fromMap(params)
                .list();
    }

    @PostMapping("/lambda")
    public List<SysUserVO> getByLambda(@RequestBody Map<String, Object> params) {
        return sysUserMapper.lambdaBooster()
                .fromMap(params)
                .eq(SysUser::getState, 1)
                .gte(SysUser::getAge, 18)
                .in(SysUser::getRoleId, Arrays.asList(1, 2))
                .like(SysUser::getUserName, "tom")
                .list();
    }

    @PostMapping("/page/{current}/{size}")
    public BPage<SysUserVO> getUserPage(@RequestBody Map<String, Object> params,
                                        @PathVariable Long current,
                                        @PathVariable Long size) {
        return sysUserMapper.lambdaBooster()
                .fromMap(params)
                .page(current, size);
    }
}
```

### 2. 实体直连 Booster

```java
import io.github.luminion.sqlbooster.core.BoosterModel;

public class SysUser implements BoosterModel<SysUser, SysUserVO> {
}

List<SysUserVO> list = new SysUser().lambdaBooster()
        .eq(SysUser::getState, 1)
        .list();
```

### 3. 使用 `EntityBoosters`

`EntityBoosters` 支持两种场景：

- 实体已实现 `BoosterModel<T, V>`，可直接按实体类获取默认 `V`
- 显式指定目标类型，按 `T + R` 获取适配后的 Booster

```java
import io.github.luminion.sqlbooster.core.EntityBoosters;

List<SysUserVO> voList = EntityBoosters.lambda(SysUser.class)
        .eq(SysUser::getState, 1)
        .list();

List<SysUserDTO> dtoList = EntityBoosters.lambda(SysUser.class, SysUserDTO.class)
        .eq(SysUser::getState, 1)
        .list();
```

### 4. 自定义 Mapper 直接接 `SqlContext`

现在 `SqlBuilder.build()` / `toSqlContext()` 默认就会完成字段映射、后缀解析和合法性处理，可以直接作为 mapper 入参：

```java
public interface SysUserCustomMapper {

    List<SysUserLiteVO> selectLite(SqlContext<SysUser> sqlContext);
}
```

```java
SqlContext<SysUser> sqlContext = SqlBuilder.of(SysUser.class)
        .fromMap(params)
        .eq(SysUser::getState, 1)
        .orderByDesc(SysUser::getId)
        .toSqlContext();

List<SysUserLiteVO> list = sysUserCustomMapper.selectLite(sqlContext);
```

对应 XML 可以直接复用内置片段：

```xml
<select id="selectLite" resultType="com.example.vo.SysUserLiteVO">
    SELECT a.id, a.user_name
    FROM sys_user a
    <where>
        <include refid="sqlbooster.conditions"/>
    </where>
    <trim prefix="ORDER BY" prefixOverrides=",">
        <include refid="sqlbooster.sorts"/>
    </trim>
</select>
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
| 包含任意指定 bit 位 | `HasAnyBits` | `"roleHasAnyBits": 4` | Number |
| 包含所有指定 bit 位 | `HasAllBits` | `"roleHasAllBits": 4` | Number |
| 不包含指定 bit 位 | `HasNoBits` | `"roleHasNoBits": 4` | Number |

#### 入参示例

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
import io.github.luminion.sqlbooster.model.SqlContext;
import io.github.luminion.sqlbooster.util.SqlContextUtils;

HashMap<String, String> customMap = new HashMap<>();
customMap.put("_like", "LIKE");
customMap.put("_ge", ">=");

// 1. 全局替换默认后缀映射
SqlContextUtils.refreshDefaultSuffixes(customMap);

// 2. 局部指定后缀映射
SqlContext<SysUser> sqlContext = SqlBuilder.of(SysUser.class)
        .fromMap(params)
        .toSqlContext(customMap);
```

### 2. Builder 安全策略

当前版本的构造器默认策略如下：

- `fromMap` / `fromBean`：宽松模式。未知字段仍会保留到 `params`。
- 兼容保留的 `appendByMap` / `appendByBean` 仍走同一套宽松规则，但已不再推荐作为主入口。
- Lambda 链式条件：严格模式。字段不存在、值类型不合法、空集合 `in/notIn` 等情况会立即抛错。
- `build()` 和 `toSqlContext()`：默认返回已经标准化的 `SqlContext`，可以直接给 mapper 使用。

```java
SqlContext<SysUser> sqlContext = SqlBuilder.of(SysUser.class)
        .fromMap(params)
        .eq(SysUser::getState, 1)
        .build();
```

### 3. BoosterInterceptor 拦截器

`BoosterInterceptor` 暴露了 `preHandle` 和 `postHandle` 方法，允许在查询执行前后对上下文及结果集进行干预。适用于逻辑删除、多租户隔离、数据脱敏等横切逻辑。

```java
@Service
public class SysUserServiceImpl extends MpServiceImpl<SysUserMapper, SysUser, SysUserVO> {

    @Override
    public void preHandle(SqlContext<SysUser> context) {
        context.getConditions().add(new Condition("deleted", "=", 0));
        if (!isAdmin()) {
            context.getConditions().add(new Condition("createBy", "=", UserUtils.getUserId()));
        }
    }

    @Override
    public void postHandle(SqlContext<SysUser> context, List<SysUserVO> resultList) {
        resultList.forEach(vo -> {
            if (vo.getPhone() != null) {
                vo.setPhone(vo.getPhone().replaceAll("(\\d{3})\\d{4}(\\d{4})", "$1****$2"));
            }
        });
    }
}
```

### 4. SqlContext 结构解析

`SqlContext` 为底层统一数据结构，支持前端以 JSON 格式提交多层级组合条件。解析时会依据实体类进行字段合法性匹配。

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
| 包含任意指定 bit 位 | `has any bits` | Number |
| 包含所有指定 bit 位 | `has all bits` | Number |
| 不包含指定 bit 位 | `has no bits` | Number |

#### 基础条件与排序

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

`SqlContext` 包含以下核心节点：

- `conditions`: 当前层级的条件节点数组
- `and`: `true` 表示 `AND`，`false` 表示 `OR`
- `next`: 子级嵌套结构
- `sorts`: 排序规则，仅首层传递

目标 SQL：

```sql
WHERE (country = 'china' AND mobile IS NOT NULL)
  AND (name = 'mike' OR name = 'john')
  AND (age < 18 OR age > 60)
```

对应 JSON：

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
