"""
Data models and ORM-like classes for BRE database
"""
from dataclasses import dataclass, field
from typing import List, Optional, Dict, Any
from datetime import datetime


@dataclass
class User:
    """User model"""
    id: Optional[int] = None
    username: str = ""
    email: str = ""
    password_hash: str = ""
    role_id: int = 0
    full_name: Optional[str] = None
    is_active: bool = True
    last_login: Optional[datetime] = None
    created_at: Optional[datetime] = None
    updated_at: Optional[datetime] = None


@dataclass
class Role:
    """Role model"""
    id: Optional[int] = None
    role_name: str = ""
    description: Optional[str] = None
    can_upload: bool = False
    can_analyze: bool = False
    can_delete: bool = False
    can_export: bool = True
    can_manage_users: bool = False
    created_at: Optional[datetime] = None


@dataclass
class Project:
    """Project model"""
    id: Optional[int] = None
    name: str = ""
    description: Optional[str] = None
    created_by: Optional[int] = None
    created_at: Optional[datetime] = None
    updated_at: Optional[datetime] = None


@dataclass
class AnalysisRun:
    """Analysis run model"""
    id: Optional[int] = None
    project_id: int = 0
    upload_id: str = ""
    upload_filename: Optional[str] = None
    uploaded_by: Optional[int] = None
    analysis_date: Optional[datetime] = None
    status: str = "pending"
    total_files: int = 0
    total_loc: int = 0
    total_sloc: int = 0
    total_comments: int = 0
    total_blank: int = 0
    avg_complexity: float = 0.0
    error_message: Optional[str] = None
    created_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None


@dataclass
class File:
    """File model"""
    id: Optional[int] = None
    analysis_run_id: int = 0
    file_path: str = ""
    file_name: str = ""
    file_type: Optional[str] = None
    file_size: Optional[int] = None
    loc: int = 0
    sloc: int = 0
    comments: int = 0
    blank: int = 0
    complexity: int = 0
    functions: int = 0
    created_at: Optional[datetime] = None


@dataclass
class Dependency:
    """Dependency model"""
    id: Optional[int] = None
    file_id: int = 0
    analysis_run_id: int = 0
    dependency_type: str = ""
    target: str = ""
    line_number: Optional[int] = None
    signature: Optional[str] = None
    description: Optional[str] = None
    parameters: List[str] = field(default_factory=list)
    created_at: Optional[datetime] = None


@dataclass
class DatabaseOperation:
    """Database operation model"""
    id: Optional[int] = None
    file_id: int = 0
    analysis_run_id: int = 0
    operation_type: str = ""
    category: Optional[str] = None
    line_number: Optional[int] = None
    query_text: Optional[str] = None
    target_table: Optional[str] = None
    target_segment: Optional[str] = None
    parameters: List[tuple] = field(default_factory=list)
    created_at: Optional[datetime] = None


@dataclass
class BusinessRule:
    """Business rule model"""
    id: Optional[int] = None
    file_id: int = 0
    analysis_run_id: int = 0
    rule_type: str = ""
    line_number: Optional[int] = None
    condition_name: Optional[str] = None
    condition_value: Optional[str] = None
    description: Optional[str] = None
    code_snippet: Optional[str] = None
    confidence_score: float = 1.0
    created_at: Optional[datetime] = None
