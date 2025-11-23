import React, { useState, useEffect } from 'react';
import {
  Box,
  Card,
  CardContent,
  Typography,
  Alert,
  CircularProgress,
  Stepper,
  Step,
  StepLabel,
  StepContent,
  Chip,
  Paper,
} from '@mui/material';
import {
  Timeline as TimelineIcon,
  CheckCircle as CheckCircleIcon,
  Error as ErrorIcon,
} from '@mui/icons-material';
import api from '../../services/api';

interface ExecutionPathViewProps {
  uploadId: string;
  fromProgram: string;
  toProgram: string;
}

const ExecutionPathView: React.FC<ExecutionPathViewProps> = ({
  uploadId,
  fromProgram,
  toProgram,
}) => {
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [pathData, setPathData] = useState<any>(null);

  useEffect(() => {
    loadExecutionPath();
  }, [uploadId, fromProgram, toProgram]);

  const loadExecutionPath = async () => {
    setLoading(true);
    setError(null);

    try {
      const data = await api.getExecutionPath(uploadId, fromProgram, toProgram);
      setPathData(data);
      setLoading(false);
    } catch (err: any) {
      setError(err.message || 'Failed to load execution path');
      setLoading(false);
    }
  };

  if (loading) {
    return (
      <Box display="flex" justifyContent="center" p={4}>
        <CircularProgress />
      </Box>
    );
  }

  if (error) {
    return (
      <Alert severity="error" sx={{ mb: 2 }}>
        {error}
      </Alert>
    );
  }

  if (!pathData) {
    return null;
  }

  if (!pathData.path_found) {
    return (
      <Alert severity="warning" icon={<ErrorIcon />}>
        <Typography variant="body1" gutterBottom>
          <strong>No Path Found</strong>
        </Typography>
        <Typography variant="body2">
          {pathData.message || `No execution path found from ${fromProgram} to ${toProgram}`}
        </Typography>
        <Typography variant="body2" sx={{ mt: 1 }}>
          This could mean:
        </Typography>
        <ul>
          <li>The programs are not connected through calls</li>
          <li>The path is indirect (more than 10 hops)</li>
          <li>One or both programs might not exist in the codebase</li>
        </ul>
      </Alert>
    );
  }

  const callDetails = pathData.call_details || [];

  return (
    <Box>
      {/* Summary Card */}
      <Card sx={{ mb: 3 }}>
        <CardContent>
          <Box display="flex" alignItems="center" gap={2} mb={2}>
            <TimelineIcon color="primary" />
            <Typography variant="h6">Execution Path</Typography>
          </Box>
          <Typography variant="body1" gutterBottom>
            <strong>From:</strong> {pathData.from_program} → <strong>To:</strong> {pathData.to_program}
          </Typography>
          <Typography variant="body2" color="textSecondary">
            Path: {pathData.path_description}
          </Typography>
          <Box mt={2}>
            <Chip
              icon={<CheckCircleIcon />}
              label={`${pathData.hops} hops`}
              color="success"
              variant="outlined"
            />
          </Box>
        </CardContent>
      </Card>

      {/* Stepper showing execution path */}
      <Card>
        <CardContent>
          <Typography variant="h6" gutterBottom>
            Call Sequence
          </Typography>
          <Stepper orientation="vertical">
            {callDetails.map((call: any, index: number) => (
              <Step key={index} active={true} completed={true}>
                <StepLabel
                  StepIconComponent={() => (
                    <Box
                      sx={{
                        width: 32,
                        height: 32,
                        borderRadius: '50%',
                        bgcolor: 'primary.main',
                        color: 'white',
                        display: 'flex',
                        alignItems: 'center',
                        justifyContent: 'center',
                        fontWeight: 'bold',
                      }}
                    >
                      {index + 1}
                    </Box>
                  )}
                >
                  <Box>
                    <Typography variant="subtitle1" fontWeight="bold">
                      {call.from_prog} → {call.to_prog}
                    </Typography>
                    <Box display="flex" gap={1} mt={0.5}>
                      <Chip label={call.call_type} size="small" color="primary" />
                      <Chip label={call.call_mechanism} size="small" variant="outlined" />
                    </Box>
                  </Box>
                </StepLabel>
                <StepContent>
                  <Paper variant="outlined" sx={{ p: 2, bgcolor: 'grey.50' }}>
                    <Typography variant="body2" gutterBottom>
                      <strong>Line:</strong> {call.caller_line_number}
                    </Typography>
                    <Typography
                      variant="caption"
                      component="pre"
                      sx={{
                        mt: 1,
                        p: 1,
                        bgcolor: 'white',
                        borderRadius: 1,
                        overflow: 'auto',
                        border: '1px solid',
                        borderColor: 'divider',
                      }}
                    >
                      {call.call_signature}
                    </Typography>
                    {call.parameters && call.parameters.length > 0 && (
                      <Box mt={1}>
                        <Typography variant="caption" color="textSecondary">
                          Parameters: {call.parameters.join(', ')}
                        </Typography>
                      </Box>
                    )}
                  </Paper>
                </StepContent>
              </Step>
            ))}

            {/* Final step - arrival at target */}
            <Step active={true} completed={true}>
              <StepLabel
                StepIconComponent={() => (
                  <CheckCircleIcon
                    sx={{ width: 32, height: 32 }}
                    color="success"
                  />
                )}
              >
                <Typography variant="subtitle1" fontWeight="bold">
                  Reached: {pathData.to_program}
                </Typography>
              </StepLabel>
            </Step>
          </Stepper>
        </CardContent>
      </Card>
    </Box>
  );
};

export default ExecutionPathView;
